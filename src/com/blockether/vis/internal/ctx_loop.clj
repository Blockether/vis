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
  [_env]
  ;; Tasks and facts are gone, so there are NO model-facing engine mutators
  ;; left to bind. done() is bound by the loop's done handler, not here.
  ;; Returns an empty map so the loop's binding merge stays a no-op.
  {})

;; =============================================================================
;; Per-iter helpers used by the loop
;; =============================================================================

(defn drain-warnings!
  "No engine warnings exist anymore (the structural-warning surface was
   removed with tasks/facts). Always returns []. Kept so the renderer's
   between-iters call site stays valid."
  [_env]
  [])

(defn apply-done!
  "Side-effecting wrapper around `eng/apply-done`. Compaction is the
   standalone `summarize` verb, not a done arg.

   Returns the intent map plus warnings."
  [{:keys [ctx-atom] :as env} {:keys [answer user-request turn-summary]}]
  (let [cursor (cursor-snapshot env)
        scope  (synthesize-scope env)]
    (when ctx-atom
      (swap! ctx-atom
        (fn [c]
          (let [c+cur (assoc c :session/scope cursor)
                {ctx' :ctx} (eng/apply-done c+cur scope
                              {:answer answer
                               :user-request user-request
                               :turn-summary turn-summary})]
            (dissoc ctx' :session/scope))))
      (tel/log! {:level :info :id ::apply-done
                 :data {:answer-present? (boolean (not (clojure.string/blank? (str answer))))}}
        "apply-done completed"))
    {:answer answer
     :blocked? false
     :warnings []}))

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
                           (seq (:routing env)) (assoc :session/routing (:routing env)))]
      ;; No structural warnings / indexes / trailer left — the renderer just
      ;; serializes the bare context. Tool results reach the model through the
      ;; loop's message history, not a ctx trailer.
      (renderer-fn {:ctx ctx* :warnings []}))))

;; =============================================================================
;; rewind / lens / find — model-facing recovery bindings
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; All DB-backed against `session_turn_iteration.forms`.
;; =============================================================================

