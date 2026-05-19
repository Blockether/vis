# Vis Session CTX Specification

Status: locked
Scope: cross-turn memory model — replaces the REPL-trailer carry with a structured EDN blob persisted in `session_state`
Goal: every cross-turn fact lives in one serialisable EDN blob (`ctx`), mutated through two operators (`ctx!`, `ctx-fn`), read as an immutable per-eval snapshot (`ctx`). No LLM-driven compaction, no engine-side recap, no headline generation.

---

## 1. Motivation

Today's cross-turn carry is a per-turn REPL trailer: the engine seeds the next provider call with `;; iter N (TURN N-K, persisted)` blocks reconstructed from prior iteration rows in SQLite, terminated by `;; ctx = …` pretty-printed env. Three concrete failures follow:

- **Goldfish on user intent.** User states "do X in style Y" → model replies → user message is dropped at turn boundary → next turn has only the model's reaction, not the directive.
- **Linear blow-up.** Every prior turn's iterations are replayed verbatim. Twenty turns of moderate work exceed any provider's working window before recapture.
- **Compaction temptation.** The instinctive fix — LLM-summarise prior turns into "headlines" — is lossy, costs an extra round-trip, and shifts the truth from raw text to model-of-the-moment interpretation.

The fix is not better compaction. The fix is structured state. Model writes durable facts (decisions, tasks, evidence, learned rules) into a typed EDN map; engine carries that map; nothing else crosses turn boundary aside from the live SCI defs already persisted.

REPL ergonomics inside a turn are preserved — chat messages still ferry `(code) → ;; => result` pairs. Only the cross-turn trailer hack disappears.

---

## 2. Vocabulary

| Term | Meaning |
|---|---|
| **ctx** | The session's structured cross-turn EDN blob. One per `session_state` row. Source of truth for everything that survives turn boundary. |
| **subtree** | A top-level key in ctx with a defined shape and lifecycle (`:specs`, `:tasks`, `:evidences`, `:insights`, `:history`, `:workspace`, `:defs`). |
| **ctx!** | Engine-bare mutator. Semantics of `assoc-in`. Single mutation primitive for values. |
| **ctx-fn** | Engine-bare mutator. Semantics of `update-in`. Single mutation primitive for transforms. |
| **ctx** (symbol) | Read-only snapshot of the current ctx, rebound by the engine before every SCI eval. Immutable inside one eval. |
| **iter** | One provider round-trip inside a turn. Iterations within a turn appear in the chat-message array; they are not stored inside ctx. |
| **turn** | One user-message → … → `(done …)` (or `:error`/`:cancelled`) cycle. Persisted in `session_turn` + `session_turn_iteration` (today's shape, unchanged). |
| **breadcrumb** | One entry in `[:history]` — a structured record of what a completed turn produced. Not a summary. Not LLM-generated. Engine-derived from diff + model-emitted `:subject`. |
| **insight** | Cross-cutting rule or preference. Standalone (no FK). `:scope ∈ {:session, :project}`. Survives session restart; project-scoped insights survive across sessions. |
| **evidence** | Append-only proof / observation / decision tied to a task or spec. Audit trail. Never compacted; pruning is manual or out-of-scope for v1. |

Banned terms in code and prose from this point: **recap**, **trailer-as-cross-turn**, **headline**, **refine-recap**. The features they named are out of scope (§13).

---

## 3. Invariants

1. **One ctx per `session_state`, serialisable as EDN, stored as `TEXT` blob.** No splits across tables. No partial views in DB.
2. **All cross-turn writes go through `ctx!` or `ctx-fn`.** No other SCI sym mutates ctx state. No engine code path bypasses these two outside the documented hooks (§9).
3. **`ctx` symbol is immutable per eval.** Engine rebinds before each iter eval; mutations within one eval do not change the bound value until the next eval.
4. **No LLM compaction of ctx.** No engine path invokes a model to shrink, summarise, or rephrase ctx content. Pruning is deterministic (per-subtree policy).
5. **Defs remain the SCI source of truth for callables.** The `[:defs]` subtree is a read-only directory view (doc, type, arity, defined-turn). The runtime fn lives in SCI; ctx never stores fns.
6. **REPL rendering is preserved within a turn.** Tool results are formatted as `;; => …`. Models still see iter-by-iter REPL flow through the chat messages array. The cross-turn REPL trailer is the thing that goes; in-turn REPL stays.
7. **Lazy fetch for archived turn detail.** Full iteration code/results for older turns are reachable through foundation tools (`v/iterations`, `v/turn-message`, `v/turn-answer`) — never preloaded into the prompt.
8. **Persistence is write-through at iter end.** Engine persists ctx on every successful iter (after `ctx!`/`ctx-fn` calls drain) and at turn end. Process crash mid-turn leaves the last successful iter's ctx intact.
9. **Schema validation is soft.** Engine validates known subtree shapes (warning on violation), never rejects. Top-level user-defined keys are free-form.

---

## 4. Schema

```clojure
;; session_state.ctx — single EDN blob
{:session/id    "01HXYZ…"          ; ULID
 :session/turn  5                   ; current turn (1-indexed)

 ;; --- Flat top-level data: model may add keys ad-hoc ---
 :style         :caveman

 ;; --- Workspace pointer (read view of workspace state) ---
 :workspace     {:trunk     :main
                 :phase     :step-2-of-4
                 :blockers  []
                 :diff-ref  "ws-diff-42"}

 ;; --- Specs: formal requirements ---
 :specs         {1 {:title       "Refactor X into PL caveman style"
                    :acceptance  ["all tests pass"
                                  "no string slop in outputs"]
                    :status      :in-progress       ; :draft :in-progress :done :cancelled
                    :added-turn  1}}

 ;; --- Tasks: work items linked to specs ---
 :tasks         {1 {:title       "implement ctx! / ctx-fn"
                    :spec-ref    1
                    :status      :done              ; :todo :in-progress :done :blocked :cancelled
                    :deps        []
                    :added-turn  2
                    :done-turn   4}
                 2 {:title       "migrate trailer→ctx"
                    :spec-ref    1
                    :status      :in-progress
                    :deps        [1]
                    :added-turn  2}}

 ;; --- Evidences: append-only proof / observations / decisions ---
 :evidences     {1 {:type        :test-passed       ; :test-passed :test-failed :decision
                    :body        "rg 'ctx!' returns 17 hits in src/"
                    :task-refs   [1]
                    :added-turn  3}
                 2 {:type        :decision
                    :body        "FIFO eviction over LLM-refine — compaction is the root of evil"
                    :task-refs   []
                    :added-turn  3}}

 ;; --- Insights: standalone rules / preferences ---
 :insights      {1 {:type   :pref
                    :body   "User prefers terse PL caveman responses"
                    :scope  :session
                    :tags   #{:style :communication}
                    :added-turn 1}
                 2 {:type   :rule
                    :body   "Tests in this repo must hit a real DB, not mocks"
                    :scope  :project
                    :tags   #{:testing}
                    :added-turn 3}}

 ;; --- History TOC: one entry per completed turn ---
 :history       [{:turn 1
                  :user-msg     "Zapamiętaj że robimy refactor X w stylu caveman."
                  :status       :done
                  :subject      "set style + initial spec"
                  :produced-ctx [[:style] [:specs 1] [:insights 1]]
                  :produced-defs []}
                 {:turn 2
                  :user-msg     "kontynuuj plan"
                  :status       :done
                  :subject      "task plan for refactor"
                  :produced-ctx [[:tasks 1] [:tasks 2]]
                  :produced-defs [build-workspace]}]

 :history/archive {:turn-ids [3 4]                  ; older turns, IDs only
                   :fetch-with "(v/turn-toc 3 4)"}

 ;; --- Defs directory (read view; runtime in SCI) ---
 :defs          {build-workspace {:doc         "Workspace builder for session-id"
                                  :type        :fn
                                  :arity       1
                                  :defined-turn 2}}

 ;; --- Counters for deterministic ID minting ---
 :counters      {:tasks 2 :specs 1 :evidences 2 :insights 2}}
```

Notes:

- All maps keyed by integer ID use `:counters` for the next-ID source. `(gen-id :tasks)` (engine-bare helper) returns `(inc (get-in ctx [:counters :tasks]))` **and** updates the counter atomically.
- `:produced-ctx` is engine-derived from a diff of ctx keys between turn start and turn end. Zero LLM, deterministic.
- `:history` holds the last 20 verbatim entries; older turns migrate to `:history/archive` (IDs + subjects only). The 20-cap is per-session config.

---

## 5. Subtree contracts

Each subtree has a fixed shape, a defined lifecycle, and a soft schema (§3 invariant 9).

### 5.1 `:specs`

```
{int → {:title       string
        :acceptance  [string]
        :status      #{:draft :in-progress :done :cancelled}
        :added-turn  int
        :done-turn   int?}}
```

Lifecycle: long-lived. Status churn rare. Specs persist for the session unless explicitly cancelled.

### 5.2 `:tasks`

```
{int → {:title       string
        :spec-ref    int?           ; FK to :specs
        :status      #{:todo :in-progress :done :blocked :cancelled}
        :deps        [int]          ; FKs to other :tasks
        :added-turn  int
        :done-turn   int?
        :blocked-on  string?}}      ; free text when :status = :blocked
```

Lifecycle: mid-lived. High status churn. Tasks complete or are cancelled within a few turns of creation.

### 5.3 `:evidences`

```
{int → {:type        #{:test-passed :test-failed :decision :measurement :reference}
        :body        string
        :task-refs   [int]          ; vec — evidence can support multiple tasks
        :added-turn  int
        :url         string?}}      ; only for :type :reference
```

Lifecycle: append-only. No update operations after creation. Pruning policy is out of scope for v1 — evidence accumulates indefinitely. Hard cap revisited if it becomes a problem.

### 5.4 `:insights`

```
{int → {:type   #{:pref :rule}
        :body   string
        :scope  #{:session :project}
        :tags   #{keyword}
        :added-turn int}}
```

Lifecycle: long-lived. `:scope :session` lives in `ctx` only. `:scope :project` is mirrored to a project-level table (§7) and merged into ctx at session start.

Hard cap: 30 entries per session, 100 per project. FIFO oldest non-pinned when exceeded. Model can also drop explicitly via `(ctx! [:insights ID] nil)`.

### 5.5 `:history`

```
{:turn int
 :user-msg     string               ; raw, truncated to 2000 chars
 :status       #{:done :error :cancelled}
 :subject      string?              ; model-emitted in (done … {:subject "…"}), optional
 :produced-ctx [[keyword|int]]      ; vec of paths added/changed this turn (engine-diff)
 :produced-defs [symbol]            ; vec of def names added this turn (engine-diff)
 :error-preview string?}            ; only for :status :error / :cancelled
```

`:history` is a vec ordered oldest-first. Last 20 verbatim; older migrate to `:history/archive` with only `{:turn-ids :fetch-with}`.

### 5.6 `:workspace`

Mirror of workspace state visible to the model. Read view of the workspace row pinned to this session; updated by workspace lifecycle hooks (`/apply-workspace-to-trunk`, etc.).

```
{:trunk     keyword                 ; :main / :master / branch name
 :phase     keyword?                ; free, model-set via ctx!
 :blockers  [string]                ; free, model-set
 :diff-ref  string?}                ; pointer to v/workspace.diff result
```

### 5.7 `:defs`

```
{symbol → {:doc          string?
           :type         #{:fn :value :macro}
           :arity        int?
           :defined-turn int}}
```

Directory only. Source of truth is the SCI process. Engine rebuilds `:defs` after each successful eval via SCI introspection.

---

## 6. Mutation API

Two engine-bare operators in every SCI eval. No other ctx mutators exist.

```clojure
(ctx! path value)
;; assoc-in semantics. Returns :ok.
;; (ctx! [:style] :caveman)
;; (ctx! [:tasks 3] {:title "…" :status :todo})

(ctx-fn path f & args)
;; update-in semantics. Returns :ok.
;; (ctx-fn [:counter] inc)
;; (ctx-fn [:tasks 3 :status] (constantly :done))
;; (ctx-fn [:evidences 5 :task-refs] conj 7)
```

Engine enforcement:

- `path` must be a vec of keywords / ints / strings.
- `value` must be EDN-serialisable. Functions are refused (use `def` instead).
- `f` in `ctx-fn` should be a pure transformation. Side effects are allowed but discouraged — engine evaluates `f` against the snapshot, then commits result.
- Both operators are atomic w.r.t. one another — engine serialises mutations through a single agent / atom inside the session.

Two engine-bare helpers around id minting:

```clojure
(gen-id subtree-key)
;; Atomically (inc (get-in ctx [:counters subtree-key])), updates :counters, returns new id.
;; (gen-id :tasks) ;; → 3, ctx[:counters :tasks] now 3

(ctx)
;; Returns the current ctx map. Distinct from the `ctx` symbol which is the
;; per-eval snapshot — (ctx) bypasses snapshot and reads the live state. Use
;; only when intentional. Prompt steers models to prefer `ctx`.
```

Read patterns use standard Clojure on the `ctx` snapshot symbol:

```clojure
(:style ctx)
(get-in ctx [:tasks 1 :status])
(->> ctx :tasks vals (filter #(= :todo (:status %))))
```

---

## 7. Persistence

### 7.1 SQLite

`session_state` gains one column:

```sql
ALTER TABLE session_state ADD COLUMN ctx TEXT NOT NULL DEFAULT '{}';
```

V1 schema rewrite — no real ALTER runs (per AGENTS.md / PLAN.md decision 4). Inline the column in the `CREATE TABLE`.

Project-scoped insights live in a separate table:

```sql
CREATE TABLE project_insight (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  repo_id     TEXT NOT NULL,
  body        TEXT NOT NULL,
  type        TEXT NOT NULL CHECK (type IN ('pref','rule')),
  tags        TEXT,                     -- EDN set
  added_at    INTEGER NOT NULL
);
CREATE INDEX idx_project_insight_repo ON project_insight(repo_id);
```

### 7.2 Write semantics

- Engine writes ctx blob to `session_state.ctx` after each successful iter that touched ctx.
- Project insights write-through on `ctx!`/`ctx-fn` that lands a `:scope :project` insight.
- Process crash mid-iter loses only that iter's mutations.

### 7.3 Restore semantics

- On session resume: load ctx blob, parse EDN.
- Merge project-scoped insights from `project_insight` table into `[:insights]` if missing.
- Topo-restore defs by re-evaluating `def` forms from `session_turn_iteration.code` (status quo).

---

## 8. Wire shape

The provider call carries exactly three message kinds: one system, one user (with ctx), and the in-turn assistant/tool ping-pong.

```text
[ {:role "system"    :content STABLE_PROMPT}

  {:role "user"      :content "<CURRENT-USER-MESSAGE>\n\n;; ctx\n<pretty-printed ctx EDN>"}

  ;; iteration 1 emitted code
  {:role "assistant" :content "(ctx! [:tasks 3] {:title \"…\" :status :todo})"}
  {:role "tool"      :content ";; => :ok"}

  ;; iteration 2
  {:role "assistant" :content "(->> ctx :tasks vals (filter …) count)"}
  {:role "tool"      :content ";; => 4"}

  ;; …

  {:role "assistant" :content "(done [:ir …] {:subject \"finished step 2\"})"}
  {:role "tool"      :content ";; => :done"} ]
```

Key properties:

- One user message per turn. CTX is appended after the user request, separated by `;; ctx` marker.
- REPL `;; => …` formatting on every tool result. Models read the chat as a live REPL session — only the cross-turn replay disappears.
- No assistant replay across turns. Provider state poison is avoided as today.

CTX pretty-print is deterministic (sorted keys, fixed indent). Long values (e.g. evidence body > 400 chars) get truncated with `…` suffix; full read available via `(get-in ctx …)`.

---

## 9. Engine integration

### 9.1 Per-eval

Before every SCI eval inside an iter, engine:

1. Reads ctx blob.
2. Binds the symbol `ctx` to that immutable snapshot in the SCI env.
3. Evaluates the code.
4. Drains pending `ctx!` / `ctx-fn` mutations through the session's mutation agent.
5. Re-derives `:defs` directory from SCI introspection of new defs.
6. Returns the eval result.

### 9.2 Turn end

On `(done …)`:

1. Engine reads model-emitted `:subject` from `done` options map.
2. Computes `:produced-ctx` = diff(ctx-at-turn-start, ctx-now).
3. Computes `:produced-defs` = diff(defs-at-turn-start, defs-now).
4. Appends `:history` entry.
5. If `:history` count > 20, migrate oldest to `:history/archive`.
6. Increments `:session/turn`.
7. Persists ctx blob.

On `:error` / `:cancelled`: same flow, `:status` reflects outcome, `:produced-ctx` / `:produced-defs` still computed from whatever landed.

### 9.3 Session start

1. Load ctx blob from `session_state.ctx`.
2. Merge project-scoped insights from `project_insight` if missing in ctx.
3. Topo-restore defs.
4. Bind ctx snapshot for first iter.

### 9.4 Hidden symbols

Extends the engine hidden-sym set with: `ctx!`, `ctx-fn`, `gen-id`, `(ctx)`. The `ctx` symbol is bound but not hidden — it shadows are intentional (`let`-bindings can override locally).

---

## 10. Prompt rules (model-facing)

The stable prompt embeds a ~30-line CTX section:

```text
You write to and read from `ctx`, a session-durable map.

READ:
  ctx                 ; immutable snapshot of current state
  (:key ctx)
  (get-in ctx [:tasks 3 :status])

WRITE (only two operators):
  (ctx! [:path] value)        ; assoc-in semantics
  (ctx-fn [:path] f & args)   ; update-in semantics

IDs are minted with (gen-id :subtree).

Conventions:
  Use ctx for data, decisions, observations, requirements, learned rules.
  Use def only for reusable callables; always with ^{:doc "…"} metadata.
  Use let / inline values for one-shot computations.

Subtrees:
  :specs         what must be delivered (with :acceptance)
  :tasks         what we are doing now (with :spec-ref, :status, :deps)
  :evidences     observed proof / decisions (append-only, link to :task-refs)
  :insights      rules / preferences (:scope :session or :project)
  :workspace     workspace state (phase, blockers, diff-ref)
  :defs          read-only directory of your defined functions
  :history       per-turn breadcrumb (engine-managed; do not write)
  :counters      engine-managed; do not write

For older turn detail use the foundation tools:
  (v/turn-message N)   ; raw user message of turn N
  (v/iterations N)     ; full iteration code+results of turn N
  (v/turn-answer N)    ; final IR of turn N
```

The 30-line limit is hard — anything longer dilutes the rest of the system prompt.

---

## 11. Foundation tools

Two new foundation tools, registered alongside `v/workspace.diff`:

```clojure
v/turn-message id     ; → string — raw user message of turn id
v/iterations id       ; → [{:position int :code string :result edn}] — full iter rows
v/turn-answer id      ; → edn — final IR of turn id
v/turn-toc from to    ; → [{:turn int :subject string?}] — light TOC for archived range
```

All four are read-only, idempotent, and queried directly against `session_turn` / `session_turn_iteration` rows. They never preload into the prompt — model invokes them only when needed.

---

## 12. Out of scope (explicit non-goals)

- LLM-driven recap / headline / refine. No engine path invokes a model to shrink ctx.
- Per-turn assistant replay across turns.
- "Pinned" insights as a separate axis. Drop / keep is the only signal; FIFO is the only eviction.
- Cross-session ctx merge beyond `:scope :project` insights. Each session starts with its own ctx.
- Schema-locked enforcement. Validation is soft (warnings) so models can experiment with new top-level keys.

---

## 13. Open questions

These are deferred to implementation, not blockers for §1–§12:

1. **`:history` per-session cap.** 20 is a guess. May need 10 or 50 once measured. Configurable: `vis.config/history-window`.
2. **Evidence pruning.** Append-only is fine until it isn't. Revisit when first session exceeds 5k tokens in `[:evidences]` alone.
3. **Project-insight scope.** `:project` is per `repo_id`. Multi-repo monorepos may need finer scope. Out of scope for v1.
4. **Soft schema warnings — surface where?** Probably as `;; ⚠ ctx warning: …` lines in iter tool results. Needs prompt rule on how to read them.
5. **Counters race.** Single SCI eval is single-threaded, so `gen-id` is naturally atomic. If we ever go concurrent (parallel agents), revisit.

---

## 14. Execution order

Each step is a separate commit. Tests stay red until step 7.

1. **Schema rewrite** — add `ctx TEXT NOT NULL DEFAULT '{}'` to `session_state` inline; add `project_insight` table.
2. **Persistance backend** — `db-session-state-set-ctx!`, `db-session-state-get-ctx`, `db-project-insight-*`.
3. **Engine primitives** — `ctx!`, `ctx-fn`, `gen-id`, `(ctx)`, `ctx` snapshot binding. Hidden-sym set extended. Mutation agent per session.
4. **Turn-end pipeline** — `:produced-ctx` / `:produced-defs` diff, history append, archive migration, ctx write-through.
5. **Foundation tools** — `v/turn-message`, `v/iterations`, `v/turn-answer`, `v/turn-toc`.
6. **Prompt rules** — embed §10 in stable prompt; remove old REPL-trailer section.
7. **Trailer removal** — drop the DB-seeded prior-iter trailer from `loop.clj` wire assembly; replace with the single user-msg shape from §8.
8. **Test migration** — new tests for ctx mutators, snapshot semantics, turn-end diff, history archive, project-insight cross-session merge.

Single push at the end of step 8 (or batched per step if midway review is desired).

---

## 15. Risks / known unknowns

- **Model adoption.** Prompt rules are necessary but not sufficient. Models trained on free-form scratchpads may resist the `ctx!` discipline. Likely needs explicit nudges in the stable prompt and a few iter-failed→reminder hooks.
- **CTX bloat from over-eager evidence.** §13 q2. Mitigation: prompt steer "record decisions and contradictions, not every observation".
- **Restore-path drift.** If SCI defs are restored but ctx blob version-skews against running code (e.g. shape change between vis versions), readers blow up. Need a `:ctx/version` key and a migration register. Out of scope for v1; flagged.
- **Provider message-cache invalidation.** Ctx changes every turn → user-message changes every turn → providers can't cache that user-message slot. System prompt remains stable and large; caching still wins net. Confirmed during prototype, but worth a benchmark gate before locking.
