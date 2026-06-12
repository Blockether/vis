(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for the CTX engine.

   The pure engine (`ctx-engine`) takes data, returns data. Real-world wiring
   needs side effects: a mutable CTX atom that lives across iters within a
   turn, a scope cursor derived from the loop's running counters, and sandbox
   symbol bindings the model can call directly from inside a fence.

   This namespace is the thin adapter that ties the engine to the loop.

   **One atom**: `:ctx-atom` on the env map carries the entire engine state
   for a session. Mutators swap! it. Transient mutator output lives on the
   ephemeral `:engine/warnings` key on the ctx itself — collected per-iter,
   drained at render. Stripped via `eng/strip-ephemeral` before
   Nippy-snapshotting.

   **`build-engine-bindings`** — `{'symbol engine-fn}` ready to merge into the
   loop's `env-bindings`. Each mutator:
     - synthesises the current form scope from the loop counters
     - calls `eng/apply-mutator`
     - swap!s the ctx atom with new ctx + accumulated warnings
     - returns `:vis/silent` so the form result isn't echoed

   Hook-emitted soft work items live as hook-sourced tasks; the model
   satisfies them via `(task-set! id {:status :done})`. Done is
   self-asserted — the engine stamps `:done-born` and surfaces a soft
   terminal-dep warning, but does not verify or revert."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
            [com.blockether.vis.internal.env-digest :as env-digest]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.resources :as resources]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Atom and constructor — ONE atom carries the entire engine state
;; =============================================================================

(defn make-ctx-atom
  "Initialize the CTX atom for a session. Uses the canonical empty scaffold.
   The scaffold carries an empty `:engine/warnings` vec so every swap!
   path can `update` it without nil-puncturing."
  ([] (atom (eng/empty-ctx)))
  ([session-id] (atom (eng/empty-ctx session-id))))

;; =============================================================================
;; Scope synthesis
;; =============================================================================

;; =============================================================================
;; Single turn-state atom
;; =============================================================================
;;
;; Replaces the six-atom soup
;;   :current-turn-position-atom
;;   :current-iteration-atom
;;   :current-form-idx-atom
;;   :current-iteration-id-atom
;;   :current-session-turn-id-atom
;;   :current-user-request-atom
;; with ONE `:turn-state-atom` holding a map of the same fields, plus
;; an `:iteration-shape` value that accepts the same flexible forms
;; (number or {:position N}).
;;
;; Reads are `(@(:turn-state-atom env) :field)` or via helpers below.
;; Writes go through `swap-turn-state!`, `set-form-idx!`, etc. — same
;; atomic update path, single source of truth.

(defn make-turn-state-atom
  "Initialize the per-session turn-state atom. Holds every cursor +
   DB-id field the iteration loop and ctx-loop helpers consume."
  []
  (atom {:turn-position   nil
         :iteration       nil
         :form-idx        nil
         :iteration-id    nil
         :session-turn-id nil
         :user-request    nil}))

(defn swap-turn-state!
  "swap! the turn-state map. Returns the new state. No-op if no atom on env."
  [env f & args]
  (when-let [a (:turn-state-atom env)]
    (apply swap! a f args)))

(defn set-turn-state!
  "swap! `assoc` shortcut for one or more turn-state keys."
  [env & kvs]
  (when-let [a (:turn-state-atom env)]
    (swap! a #(apply assoc % kvs))))

(defn read-turn-state
  "Deref the turn-state map or {} when atom is missing."
  [env]
  (or (some-> (:turn-state-atom env) deref) {}))

(defn- normalize-iteration
  "Iteration field accepts a number, a {:position N} map, or nil. Returns N."
  [v]
  (cond
    (map? v)     (or (:position v) 1)
    (number? v)  v
    :else        1))

(defn synthesize-scope
  "Build the current form scope `tN/iM/fK` from `:turn-state-atom`.
   Defaults each field to 1 (form-idx defaults to 0 → next-form 1) so
   the helper is safe to call before the loop has initialised the
   atom (e.g. early hooks)."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    (str "t" (or turn-position 1)
      "/i" (normalize-iteration iteration)
      "/f" (inc (or form-idx 0)))))

(defn cursor-snapshot
  "Build a `:session/scope` map from `:turn-state-atom`. Mirrors the
   engine's `{:turn :iter :next-form}` shape used by `classify-scope`
   and the renderer."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    {:turn      (or turn-position 1)
     :iter      (normalize-iteration iteration)
     :next-form (inc (or form-idx 0))}))

;; =============================================================================
;; Mutator bindings
;; =============================================================================

(defn apply-and-record!
  "Call the engine mutator and swap! the result onto ctx-atom in one shot.
   Engine warnings land on `:engine/warnings` directly. Returns
   `:vis/silent` so the form result is hidden from the model's eval echo
   (the mutation effect is visible on next render).

   Public so the loop can route foundation hook-task emissions through
   the same write path the model uses."
  [{:keys [ctx-atom] :as env} mutator args]
  (let [scope (synthesize-scope env)]
    (swap! ctx-atom
      (fn [c]
        (let [c+cursor (assoc c :session/scope (cursor-snapshot env))
              {:keys [ctx warnings stamped?]} (eng/apply-mutator c+cursor scope mutator args)
              base (if stamped? ctx c)]
          (cond-> base
            (seq warnings) (update :engine/warnings (fnil into []) warnings)))))
    ;; Canonical silent sentinel = the Python-native string. Mutators are
    ;; reached as Python callables; a keyword return would snake to this same
    ;; string crossing `->py`, and the engine compares "vis_silent".
    "vis_silent"))

(def ^:private engine-op-card!*
  "Lazily resolved `extension/record-engine-op-card!`. Resolved via
   `requiring-resolve` inside a delay so this ns gains no require edge on
   the extension registry (mirrors the env_python form-idx pattern)."
  (delay (try (requiring-resolve
                'com.blockether.vis.internal.extension/record-engine-op-card!)
           (catch Throwable _ nil))))

(defn- op-card!
  "Best-effort engine-verb op card. Render failures are swallowed - the
   mutation already applied; a card must never fail the verb."
  [m]
  (when-let [f @engine-op-card!*]
    (try (f m) (catch Throwable _ nil)))
  nil)

(defn- clip-str
  "Trimmed `s` cut to at most `n` chars (ASCII ellipsis suffix when cut)."
  [s n]
  (let [s (.trim ^String (str (or s "")))]
    (if (> (count s) n) (str (subs s 0 n) "...") s)))

(defn- status-label
  "Human label for a plan-step status: normalized keyword name when the
   engine recognizes it, else the raw value as a string."
  [status]
  (if-let [st (eng/normalize-plan-status status)] (name st) (str status)))

(defn- step-glyph
  "One-glyph status marker for a plan card row."
  [status]
  (case (eng/normalize-plan-status status)
    :done      "[x]"
    :doing     "[>]"
    :candidate "[?]"
    :rejected  "[!]"
    :cancelled "[-]"
    "[ ]"))

(defn- update-plan-card!
  "Card for `update_plan(steps[, scope])`: PLAN head + one glyphed row per
   step so channels show the plan shape instead of the raw Python list."
  [steps scope]
  (let [rows (filterv map? (or steps []))]
    (op-card!
      {:op     :ctx/update-plan
       :form   (pr-str (list 'update_plan (count rows)))
       :header [[:strong {} "PLAN"]
                [:span {} (str " " (count rows)
                            " step" (when (not= 1 (count rows)) "s")
                            (when scope (str " under " scope)))]]
       :body   (mapv (fn [s]
                       [:p {} [:span {} (str (step-glyph (:status s)) " "
                                          (clip-str (or (:step s) (:title s) (:key s)) 110)
                                          "  - " (status-label (:status s)))]])
                 rows)})))

(defn- plan-step-card!
  "Card for `plan_step(k, partial)`: STEP key -> status (+ verified) with
   the evidence folded under it."
  [k partial]
  (let [status   (:status partial)
        verified (:verified partial)
        arrow    (cond-> (if status (str " -> " (status-label status)) " updated")
                   verified (str " (verified)"))]
    (op-card!
      {:op     :ctx/plan-step
       :form   (pr-str (list 'plan_step (str k)))
       :header [[:strong {} "STEP"] [:span {} " "]
                [:c {} (str k)]
                [:span {} arrow]]
       :body   (when-let [ev (some-> (:evidence partial) str)]
                 [[:p {} [:span {} (str "evidence: " (clip-str ev 240))]]])})))

(defn- fact-set-card!
  "Card for `fact_set(k, partial)`: FACT key with a clipped content
   preview folded under it."
  [k partial]
  (let [content (when (map? partial) (:content partial))]
    (op-card!
      {:op     :ctx/fact-set
       :form   (pr-str (list 'fact_set (str k)))
       :header [[:strong {} "FACT"] [:span {} " "]
                [:c {} (str k)]]
       :body   (when-let [c (some-> content str)]
                 [[:p {} [:span {} (clip-str c 240)]]])})))

(defn build-engine-bindings
  "Return `{'symbol bare-fn}` for every model-facing engine mutator. The model
   writes `update_plan([...])` or `fact_set(k, {...})` directly in its Python and
   we route the call through `apply-and-record!` against the single ctx-atom.

   All mutators return `:vis/silent` - engine mutations are 'effect-only',
   visible on next render but quiet in the form echo. Each verb ALSO records
   a render-sink op card (`record-engine-op-card!`) so channels paint the
   mutation as a nice card instead of the raw Python call.

   Tasks: the ONE task verb is `update_plan` (ordered plan, Codex-style). The
   internal `:task-set!` mutator is NOT bound here - it stays engine-private for
   foundation hook-tasks.

   Facts: the ONE fact verb is `fact_set`. Relations are DECLARATIVE FIELDS on its
   map - `fact_set(k, {depends_on: [...], contradicts: [...]})` - which REPLACE the
   edge set and reconcile the symmetric contradiction back-links. The old
   `fact_depends` / `fact_contradicts` / `fact_contradicts_remove` verbs were pure
   surface duplication of that one capability and are NO LONGER bound (the engine
   mutators stay as internal primitives that `fact_set` fronts)."
  [env]
  ;; `:cli` is the NON-INTERACTIVE one-shot channel - a `candidate` proposal
  ;; can never be approved, so it must not exist: coerce candidate -> todo at
  ;; the verb boundary (BEFORE the engine normalizes), turning a proposal into
  ;; REAL open work the done-gate enforces. The prompt override already tells
  ;; the model to plan-and-execute; this is the host-side backstop so a stray
  ;; candidate can't stall the run. Interactive `:tui`/`:web` keep candidates.
  (let [cli?         (= :cli (:channel env))
        decand       (fn [step]
                       (if (and cli? (map? step)
                             (= :candidate (eng/normalize-plan-status (:status step))))
                         (assoc step :status "todo")
                         step))
        decand-steps (fn [steps]
                       (if (and cli? (sequential? steps)) (mapv decand steps) steps))]
    {;; ONE plan verb: update_plan(steps) replaces the whole plan; an optional
     ;; second positional scope key (update_plan(steps, "parent_key")) scopes the
     ;; replace to that node's subtree. No separate update_subplan.
     'update-plan!  (fn update-plan! [steps & [scope]]
                      (let [steps' (decand-steps steps)
                            res    (apply-and-record! env :update-plan! [steps' scope])]
                        (update-plan-card! steps' scope)
                        res))
     'plan-step!    (fn plan-step! [k partial]
                      (let [p   (decand partial)
                            res (apply-and-record! env :plan-step! [k p])]
                        (plan-step-card! k p)
                        res))
     ;; ONE fact verb: fact_set. depends_on + contradicts ride as declarative fields.
     'fact-set!     (fn fact-set! [k partial]
                      (let [res (apply-and-record! env :fact-set! [k partial])]
                        (fact-set-card! k partial)
                        res))}))

;; =============================================================================
;; Per-iter helpers used by the loop
;; =============================================================================

(defn drain-warnings!
  "Atomically read and clear `:engine/warnings` on ctx-atom. Called by the
   renderer between iters."
  [{:keys [ctx-atom]}]
  (when ctx-atom
    (let [ws (atom nil)]
      (swap! ctx-atom
        (fn [c]
          (reset! ws (or (:engine/warnings c) []))
          (assoc c :engine/warnings [])))
      @ws)))

(defn apply-done!
  "Side-effecting wrapper around `eng/apply-done`. Compaction is the
   standalone `summarize` verb, not a done arg.

   Returns the intent map plus warnings."
  [{:keys [ctx-atom] :as env} {:keys [answer
                                      user-request turn-summary]}]
  (let [start-ms      (System/nanoTime)
        cursor        (cursor-snapshot env)
        scope         (synthesize-scope env)
        warns         (atom [])
        blocked?      (atom false)]
    (when ctx-atom
      (swap! ctx-atom
        (fn [c]
          (let [c+cur (assoc c :session/scope cursor)
                {ctx' :ctx ws :warnings blocked :blocked?}
                (eng/apply-done c+cur scope
                  {:answer         answer
                   :user-request   user-request
                   :turn-summary   turn-summary})]
            (swap! warns into ws)
            (reset! blocked? (boolean blocked))
            (cond-> (dissoc ctx' :session/scope)
              (seq ws) (update :engine/warnings (fnil into []) ws)))))
      (tel/log! {:level :info :id ::apply-done
                 :data {:answer-present?   (boolean (not (clojure.string/blank? (str answer))))
                        :warnings          @warns
                        :blocked?          @blocked?
                        :duration-ms       (/ (- (System/nanoTime) start-ms) 1e6)}}
        "apply-done completed"))
    {:answer answer
     :blocked? @blocked?
     :warnings @warns}))

(defn stamp-cursor
  "Return a ctx map with both `:session/turn` and `:session/scope` synced
   from the loop's running counters. Render path + every engine derivation
   call goes through this so the model never sees a stale top-level
   `:session/turn` (e.g. after a resume that loaded turn N's snapshot but
   the current loop is on turn N+1)."
  [env ctx]
  (let [cursor (cursor-snapshot env)]
    (-> ctx
      (assoc :session/turn  (:turn cursor))
      (assoc :session/scope cursor))))

(defn current-ctx
  "Deref the CTX atom with both `:session/turn` and `:session/scope`
   stamped from the loop counters. This is the shape passed to the
   renderer + `derive-warnings`. Returns nil when ctx-atom is missing
   on env (defensive for partial test envs)."
  [{:keys [ctx-atom] :as env}]
  (when ctx-atom (stamp-cursor env @ctx-atom)))

(defn session-archived
  "USER-facing read of `:session/archived` — the GC'd terminal entities
   (summarize'd / TTL-archived tasks + facts, keyed by stable `:id`,
   each carrying `:vis/kind` + `:vis/key`). Deliberately NOT part of
   `session-snapshot`/`session-view`: the MODEL must never see the
   archive inline (that would undo compaction; `recall` is its only
   path back). Channels surface it to the USER in their context
   viewers — the TUI F2 panel and the web rail."
  [env]
  (when-let [ctx (current-ctx env)]
    (or (:session/archived ctx) {})))

(defn session-snapshot
  "Read-only data mirror of the `;; ctx` EDN the model reads — the value
   bound to the bare `ctx` symbol in the sandbox (re-interned before each
   eval, see loop/execute-code).

   Built to MATCH the rendered shape, NOT the raw atom: the atom stores
   `:engine/utilization` (the renderer projects it to
   `:session/utilization`) and carries `:engine/*` bookkeeping the model
   must never see. So we:
     - stamp the live cursor (`current-ctx`)
     - attach the cheap base `:session/env` digest (host/project/ext
       counts — NO extension `:ext/ctx` hooks, so this is side-effect-free
       and safe to call per-block)
     - keep ONLY `:session/*` keys
     - re-key `:engine/utilization` → `:session/utilization`

   The shape projection itself is NOT done here — it goes through the
   ONE canonical `eng/session-view`, the same fn the EDN renderer uses, so
   the bound `ctx` and the rendered `;; ctx` text cannot drift. This fn
   only supplies the LIGHT per-block enrichment (live cursor + cheap base
   `:session/env`; no derive-warnings/indexes/`:ext/ctx` hooks).

   The result is an immutable map with ZERO connection to `:ctx-atom`:
   `(assoc ctx …)` is a throwaway, and engine state changes only through
   the mutators (`task-set!`/`fact-set!` → `apply-and-record!` → swap!).
   That is the read-only guarantee. Returns nil when ctx-atom is absent."
  [env]
  (when-let [ctx (current-ctx env)]
    (let [env-block (try (env-digest/base-digest env) (catch Throwable _ nil))
          ;; Session-scoped live resources — same registry the footer reads, so
          ;; `context["resources"]` and the footer can never disagree.
          rsrc      (try (resources/list-resources (:session-id env)) (catch Throwable _ nil))]
      (eng/session-view (cond-> ctx
                          (seq env-block)      (assoc :session/env env-block)
                          (seq rsrc)           (assoc :session/resources rsrc)
                          ;; MUST mirror render-block! — the routing digest is in
                          ;; the rendered `# ctx` TEXT, so it has to be in the BOUND
                          ;; `context` dict too, else `context["routing"]`
                          ;; KeyErrors even though the model can SEE it in the text.
                          (seq (:routing env)) (assoc :session/routing (:routing env)))))))

(defn trailer->form-results
  "Flatten every trailer pin's :forms vec into a `{scope envelope}` map.
   Used at render time so engine's `classify-scope` pass can look up
   per-form `:result` / `:error` for any scope the model references."
  [trailer]
  (into {}
    (for [entry trailer
          :when (some? (:forms entry))
          form  (:forms entry)
          :when (and (some? form) (some? (:scope form)))]
      [(:scope form) form])))

(defn render-block!
  "Build the `;; ctx` block for the next user message. Pure data flow,
   single side effect (drain-warnings!) at the start. Returns the
   rendered string, or nil when ctx-atom is missing on env.

   Flow:
     deref ctx-atom → stamp cursor → attach `:session/env` digest +
     extension `:ext/ctx` contributions → build form-results map from
     trailer → build indexes → drain mutator warnings + derive
     render-time warnings → assoc `:session/hints` → call renderer.

   `:session/env` lives on the rendered ctx but is NOT pushed back into
   `ctx-atom`: extension contributions are recomputed each iter so
   transient state (e.g. voice / git status) stays fresh."
  [env renderer-fn]
  (when-let [ctx (current-ctx env)]
    (let [active-exts    (try (prompt/active-extensions env)
                           (catch Throwable _ nil))
          ext-ctx        (try (extension/ctx-contributions env active-exts)
                           (catch Throwable t
                             (tel/log! {:level :warn :id ::ctx-contributions-failed
                                        :data  {:error (ex-message t)}})
                             {}))
          env-block      (try (env-digest/deep-merge
                                (env-digest/base-digest env)
                                (:session/env ext-ctx))
                           (catch Throwable t
                             (tel/log! {:level :warn :id ::env-digest-failed
                                        :data  {:error (ex-message t)}})
                             nil))
          ;; Session-scoped managed resources (nREPLs, daemons, …). Computed
          ;; fresh each render like :session/env so the model + footer see live
          ;; lifecycle without pushing transient state into the ctx-atom.
          rsrc           (try (resources/list-resources (:session-id env)) (catch Throwable _ nil))
          ctx*           (cond-> (env-digest/deep-merge ctx (dissoc ext-ctx :session/env))
                           (seq env-block)      (assoc :session/env env-block)
                           (seq rsrc)           (assoc :session/resources rsrc)
                           ;; current model + available models, so the agent can
                           ;; route a sub_loop child by cost (read-only).
                           (seq (:routing env)) (assoc :session/routing (:routing env)))
          fr             (trailer->form-results (:session/trailer ctx*))
          idx            (eng/build-indexes ctx*)
          drained-warns  (drain-warnings! env)
          derived-warns  (eng/derive-warnings ctx* idx fr)
          warns          (vec (concat drained-warns derived-warns))
          ;; `:session/hints` is owned by `eng/session-view` (the single
          ;; projection): it wraps these structural `warns` into hint maps
          ;; and conjoins the extension hook hints. We pass the raw `warns`
          ;; as `:warnings`; we do NOT assoc `:session/hints` here, so the
          ;; data map and the rendered string can never disagree on shape.
          ctx-rendered     ctx*]
      (renderer-fn {:ctx ctx-rendered :warnings warns}))))

;; =============================================================================
;; rewind / lens / find — model-facing recovery bindings
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; All DB-backed against `session_turn_iteration.forms`.
;; =============================================================================

(defn- parse-iter-scope [s]
  (when (string? s)
    (when-let [[_ n m] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m)})))

(defn- parse-form-scope [s]
  (when (string? s)
    (when-let [[_ n m k] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)/f([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m) :form (parse-long k)})))

(defn- turn-by-pos
  "Resolve turn position to the persisted turn-soul map (or nil)."
  [{:keys [db-info session-id]} turn-pos]
  (when (and db-info session-id turn-pos)
    (some #(when (= turn-pos (:position %)) %)
      (persistance/db-list-session-turns db-info session-id))))

(defn- iter-by-pos
  "Resolve iter position to the persisted iteration map for `turn-pos`."
  [env turn-pos iter-pos]
  (when-let [turn (turn-by-pos env turn-pos)]
    (some #(when (= iter-pos (:position %)) %)
      (persistance/db-list-session-turn-iterations (:db-info env) (:id turn)))))

(defn build-introspect-bindings
  "Return `{'symbol bare-fn}` for the recovery surface: `summarize`
   (mid-turn compaction) + `recall` (the single recovery verb).

   `recall` dispatches on arg shape — by ADDRESS (form scope / entity
   key) it char-windows the stored value (scrollable, read-only); by
   CONTENT (`{:match …}`) it FTS5-searches the live (non-summarized)
   trace and returns scopes (`:match` required, `:limit` default 10).

   `_history-loader` is accepted for call-site compatibility and unused
   (cross-turn snapshot verbs were dropped)."
  [env _history-loader]
  (let [db   (:db-info env)
        sid  (:session-id env)
        live-ctx (fn [] (some-> (:ctx-atom env) deref))
        form-envelope (fn [scope]
                        (when-let [{:keys [turn iter form]} (parse-form-scope scope)]
                          (when-let [it (iter-by-pos env turn iter)]
                            (nth (vec (:forms it)) (dec form) nil))))
        addr-value (fn [addr]
                     ;; In the Python sandbox the agent addresses by STRING:
                     ;; `recall("t3/i1/f2")` (form scope) or `recall("calc_add")`
                     ;; (fact/task key). A form scope routes to that form's
                     ;; result; any other string is an entity key. STRINGS
                     ;; ONLY — ctx keys are strings (snake boundary), so a
                     ;; keyword lookup could never match anything anyway.
                     (let [c (live-ctx)
                           entity (fn [k] (or (get-in c [:session/facts k :content])
                                            (get-in c [:session/facts k])
                                            (get-in c [:session/tasks k])))]
                       (cond
                         (and (string? addr) (parse-form-scope addr)) (some-> (form-envelope addr) :result)
                         (string? addr)  (entity addr)
                         :else nil)))
        iter-pin (fn [scope]
                   ;; build a trailer pin re-materialising ALL forms of an
                   ;; iter scope "tN/iM" (for recall {:scopes …}). The DB
                   ;; rows carry the FULL envelopes (channel slice, host
                   ;; metadata); the re-pinned trailer copy gets the same
                   ;; model projection live pins get.
                   (when-let [{:keys [turn iter]} (parse-iter-scope scope)]
                     (when-let [it (iter-by-pos env turn iter)]
                       (let [forms (mapv eng/model-form-envelope (:forms it))]
                         (when (seq forms)
                           {:scope (str "t" turn "/i" iter) :forms forms})))))]
    {'task-subtree
     ;; READ helper: the focus task + its transitive descendants from the LIVE
     ;; plan tree (by `:parent` edges), as a `{key task}` dict — the FOCUSED slice
     ;; to hand a `sub_loop`/`parallel` child, so you don't reimplement the walk
     ;; or dump the whole task map. `task_subtree("oauth")` in Python.
     (fn task-subtree [root]
       (eng/subtree-of (:session/tasks (live-ctx)) (str root)))
     'summarize
     ;; The ONLY compaction path (there is no done :summarize): collapse
     ;; irrelevant trailer ranges / settled facts+tasks NOW so stale forms
     ;; don't ride every prompt until close-of-turn. Batch one right before
     ;; (done …) in the same fence to compact at close.
     (fn summarize [spec]
       (let [scope (synthesize-scope env)
             out   (atom nil)]
         (when-let [ca (:ctx-atom env)]
           (swap! ca (fn [c]
                       (let [r (eng/apply-summarize c scope (or spec {}))]
                         (reset! out r)
                         (cond-> (:ctx r)
                           (seq (:warnings r))
                           (update :engine/warnings (fnil into []) (:warnings r)))))))
         ;; Lean ack — this value rides the trailer into EVERY later
         ;; prompt. The spec is already verbatim in the form's `:src`
         ;; one line above (echoing it doubled the tokens), an empty
         ;; `:warnings` vector is noise, and `:trailer-size` is
         ;; bookkeeping the model can see from the trailer itself.
         ;; SILENT on clean success — the fold IS visible (the stub pin
         ;; replaces the folded range; archived entities leave live ctx),
         ;; and the spec is verbatim in the form's :src. Pinning an ack
         ;; map re-shipped it on every later prompt for zero signal.
         ;; Warnings still surface (the one thing not visible elsewhere).
         (if (seq (:warnings @out))
           {:warnings (vec (:warnings @out))}
           "vis_silent")))
     'recall
     ;; ONE recovery verb. Pull something that already exists back from
     ;; execution memory — dispatched on arg shape:
     ;;   RESTORE  {:ids [:t3/auth …] :why "…"}   entity → live subtree
     ;;            {:scopes ["t4/i2" …] :why "…"} iter   → trailer
     ;;            (:why REQUIRED; stamps :recalled so it's clear WHY)
     ;;   SEARCH   {:match "…"}                    → [{:scope :preview :rank} …
     ;;            {:archived <key> :kind :preview :rank} …] (archive hits
     ;;            are restorable via {:ids [<key>]})
     ;;   WINDOW   "tN/iM/fK" / :K [{:offset N}]   → scrollable slice
     (fn recall
       ([arg] (recall arg nil))
       ([arg opts]
        (let [v      (addr-value arg)
              offset (when (map? opts) (:offset opts))
              limit  (when (map? opts) (:limit opts))]
          (cond
            ;; --- restore mode (entities → subtree, scopes → trailer) ----
            (and (map? arg) (or (contains? arg :ids) (contains? arg :scopes)))
            (let [{:keys [ids scopes why]} arg]
              (if (str/blank? (str why))
                {:vis/error :recall-requires-why
                 :hint "recall({\"ids\": [...], \"why\": \"…\"}) — \"why\" is REQUIRED to restore (say why it's back)"}
                (let [scope (synthesize-scope env)
                      ids-out    (atom [])
                      scopes-out (atom [])
                      ;; recall-entity resolves LIVE (same-turn :archived)
                      ;; OR :session/archived (GC'd in a past turn — final
                      ;; state captured AT gc, in-memory). One map lookup,
                      ;; no snapshot scan.
                      restore-id
                      (fn [token]
                        ;; Python hands us the entity KEY (a string). Resolve it
                        ;; to the internal :id recall-entity matches on: a live
                        ;; fact/task key, else an archived entry by its stored
                        ;; :vis/key, else assume the token already IS an id.
                        (let [c   (live-ctx)
                              id  (or (get-in c [:session/facts token :id])
                                    (get-in c [:session/tasks token :id])
                                    (some (fn [[aid e]]
                                            (when (= (str token) (str (:vis/key e))) aid))
                                      (:session/archived c))
                                    token)
                              out (atom nil)]
                          (when-let [ca (:ctx-atom env)]
                            (swap! ca (fn [c]
                                        (let [r (eng/recall-entity c id scope (str why))]
                                          (reset! out r)
                                          (:ctx r)))))
                          (if (:found? @out)
                            {:id token :restored (:kind @out) :key (:key @out)}
                            {:id token :vis/error :not-found})))]
                  (doseq [id (or ids [])]
                    (swap! ids-out conj (restore-id id)))
                  (doseq [sc (or scopes [])]
                    (if-let [pin (iter-pin sc)]
                      (do
                        (when-let [ca (:ctx-atom env)]
                          (swap! ca update :session/trailer
                            (fn [tr] (eng/sort-trailer (conj (vec tr) pin)))))
                        (swap! scopes-out conj {:scope sc :forms (count (:forms pin))}))
                      (swap! scopes-out conj {:scope sc :vis/error :scope-not-found})))
                  ;; SILENT on full success — restored entities reappear in
                  ;; live ctx and re-pinned scopes reappear in the trailer,
                  ;; both visible next render; pinning an ack repeated that.
                  ;; Any failure keeps the per-id/scope report (the errors
                  ;; are the one thing not visible elsewhere).
                  (if (some :vis/error (concat @ids-out @scopes-out))
                    {:recalled {:ids @ids-out :scopes @scopes-out}}
                    "vis_silent"))))

            ;; --- search mode -------------------------------------------
            (and (map? arg) (contains? arg :match))
            ;; full-snake sandbox → `{"scope_after" …}` arrives as :scope_after.
            (let [match        (:match arg)
                  limit        (:limit arg)
                  scope-after  (or (:scope-after arg) (:scope_after arg))
                  empty-match? (or (nil? match)
                                 (and (string? match) (str/blank? match))
                                 (and (map? match) (empty? match)))]
              (if empty-match?
                {:vis/error :recall-requires-match
                 :hint "recall({\"match\": …}) is REQUIRED — a string, or a DSL like {\"all\": [\"patch\", \"auth\"]}"}
                (try
                  ;; Thin orchestration: the backend RENDERS the neutral query
                  ;; DSL (`match` is a string or a DSL map) to native FTS; we run
                  ;; it and hand the hits to the PURE resolver. No query-mode,
                  ;; no fallback — escaped DSL terms can't break the query.
                  (let [hits       (persistance/db-search db match
                                     ;; "code" = what the model wrote; "errors" =
                                     ;; per-form failure text indexed at iteration
                                     ;; persist — so "what failed earlier?" is
                                     ;; answerable (it wasn't: failures lived only
                                     ;; in the Nippy :forms blob, and a search for
                                     ;; an error message returned nothing).
                                     {:owner-table "session_turn_iteration"
                                      :field       ["code" "errors"]
                                      :limit       (max 1 (long (or limit 10)))})
                        turns      (or (persistance/db-list-session-turns db sid) [])
                        iter-cache (atom {})
                        iters-of   (fn [tid]
                                     (or (get @iter-cache tid)
                                       (let [rows (persistance/db-list-session-turn-iterations db tid)]
                                         (swap! iter-cache assoc tid rows)
                                         rows)))
                        cursor     (when (string? scope-after) (parse-iter-scope scope-after))
                        ;; ARCHIVED entities (GC'd / summarize'd / plan-replace-
                        ;; dropped tasks+facts) are FTS-indexed under owner
                        ;; 'archive' but were INVISIBLE to search — the model
                        ;; could restore one by key yet never FIND one by
                        ;; content. Search them too; each hit resolves through
                        ;; db-list-archive (this session's rows only — foreign-
                        ;; session hits miss the map and drop) to a restorable
                        ;; pointer: recall({"ids": ["<key>"], "why": …}).
                        ss-id      (persistance/db-latest-session-state-id db sid)
                        arch-hits  (when ss-id
                                     (persistance/db-search db match
                                       {:owner-table "archive"
                                        :limit       (max 1 (long (or limit 10)))}))
                        arch-map   (when (seq arch-hits)
                                     (persistance/db-list-archive db ss-id))
                        archived   (into []
                                     (keep (fn [{:keys [owner-id snippet rank]}]
                                             ;; owner-id is the scoped row PK
                                             ;; "<ss-uuid>/<logical-id>"; the ss
                                             ;; uuid never contains "/", the
                                             ;; logical id may (form scopes).
                                             (let [lid (second (str/split (str owner-id) #"/" 2))
                                                   e   (get arch-map lid)]
                                               (when e
                                                 {:archived (str (or (:vis/key e) lid))
                                                  :kind     (some-> (:vis/kind e) name)
                                                  :preview  snippet
                                                  :rank     rank}))))
                                     arch-hits)]
                    (into (eng/search-hits->scopes (or hits []) turns iters-of cursor)
                      archived))
                  (catch Exception e
                    {:vis/error :recall-search-failed
                     :query     match
                     :hint      (str "search failed: " (ex-message e)
                                  ". `match` is a string (implicit-AND of its words) "
                                  "or a DSL map: {\"all\":[…]}, {\"any\":[…]}, "
                                  "{\"phrase\":\"…\"}, {\"prefix\":\"…\"}, "
                                  "{\"near\":{\"terms\":[…],\"within\":k}}, or "
                                  "{\"not\":…} inside an {\"all\":…}.")}))))

            :else
            (if (nil? v)
              {:vis/error :recall-target-not-found :addr arg
               :hint "recall(\"tN/iM/fK\") windows a form result; recall(\"<fact_or_task_key>\") windows an entity"}
              ;; pass a clean Python address (no pr-str quoting) so the echoed
              ;; vis_recall / vis_next continuation read as real Python calls.
              (let [py-addr (-> (if (keyword? arg) (name arg) (str arg))
                              (str/replace "-" "_"))
                    ;; the recalled form's OWN source — render-form-value
                    ;; dispatches on its call head, so the window shows the
                    ;; SAME compressed text the original pin did (cat/rg
                    ;; gutters, shell/git model renders, raw strings) and
                    ;; never a pr-str'd Clojure map. Entity values pass a
                    ;; nil src (content strings render raw, maps as the
                    ;; canonical Python dict).
                    src     (when (and (string? arg) (parse-form-scope arg))
                              (:src (form-envelope arg)))]
                (eng/recall-window py-addr v offset limit
                  (fn [val] (ctx-renderer/render-form-value src val)))))))))}))

