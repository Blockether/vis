(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for the CTX engine.

   The pure engine (`ctx-engine`) takes data, returns data. Real-world wiring
   needs side effects: a mutable CTX atom that lives across iters within a
   turn, a scope cursor derived from the loop's running counters, and SCI
   symbol bindings the model can call directly from inside a fence.

   This namespace is the thin adapter that ties the engine to the loop.

   **One atom**: `:ctx-atom` on the env map carries the entire engine state
   for a session. Mutators swap! it. Transient mutator output lives on the
   ephemeral `:engine/warnings` key on the ctx itself — collected per-iter,
   drained at render. Stripped via `eng/strip-ephemeral` before
   Nippy-snapshotting.

   **`build-sci-bindings`** — `{'symbol sci-fn}` ready to merge into the
   loop's `env-bindings`. Each mutator:
     - synthesises the current form scope from the loop counters
     - calls `eng/apply-mutator`
     - swap!s the ctx atom with new ctx + accumulated warnings
     - returns `:vis/silent` so the form result isn't echoed

   D12: hint-satisfaction surface (`satisfy-hint!`,
   `drain-and-apply-satisfies!`, `:engine/pending-satisfies`) was retired.
   Hook-emitted soft work items now live as hook-sourced tasks; the model
   satisfies them via `(task-set! id {:status :done :proof \"…\"})` and the
   engine reconciles at end-of-iter via `eng/reconcile-done-hook-tasks`."
  (:require [com.blockether.vis.internal.ctx-engine :as eng]
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

(defn- read-or
  "Deref atom or return default. Loop atoms may be unset early in turn."
  [a default]
  (if a (or (deref a) default) default))

(defn synthesize-scope
  "Build the current form scope `tN/iM/fK` from the loop's running counters.

   Inputs (all atoms on env):
     :current-turn-position-atom    → N
     :current-iteration-atom        → {:position M} (or M directly)
     :current-form-idx-atom         → K (0-based; we add 1 for scope)

   When any counter is missing, defaults to 1. This keeps mutators safe to
   call before the loop has fully initialised the cursor (e.g. early hooks)."
  [{:keys [current-turn-position-atom
           current-iteration-atom
           current-form-idx-atom]}]
  (let [turn (read-or current-turn-position-atom 1)
        iter-raw (read-or current-iteration-atom 1)
        iter (cond
               (map? iter-raw)        (or (:position iter-raw) 1)
               (number? iter-raw)     iter-raw
               :else                  1)
        form-idx (read-or current-form-idx-atom 0)
        form (inc form-idx)]
    (str "t" turn "/i" iter "/f" form)))

(defn cursor-snapshot
  "Build a `:session/scope` map from the loop's atoms. Mirrors the engine's
   `{:turn :iter :next-form}` shape used by `classify-scope` and the renderer."
  [env]
  (let [turn (read-or (:current-turn-position-atom env) 1)
        iter-raw (read-or (:current-iteration-atom env) 1)
        iter (cond (map? iter-raw)    (or (:position iter-raw) 1)
               (number? iter-raw) iter-raw
               :else              1)
        form-idx (read-or (:current-form-idx-atom env) 0)]
    {:turn turn :iter iter :next-form (inc form-idx)}))

;; =============================================================================
;; Mutator bindings
;; =============================================================================

(defn apply-and-record!
  "Call the engine mutator and swap! the result onto ctx-atom in one shot.
   Engine warnings land on `:engine/warnings` directly. Returns
   `:vis/silent` so the form result is hidden from the model's eval echo
   (the mutation effect is visible on next render).

   Public so the loop can route foundation hook-task emissions (D12)
   through the same write path the model uses."
  [{:keys [ctx-atom] :as env} mutator args]
  (let [scope (synthesize-scope env)]
    (swap! ctx-atom
      (fn [c]
        (let [c+cursor (assoc c :session/scope (cursor-snapshot env))
              {:keys [ctx warnings stamped?]} (eng/apply-mutator c+cursor scope mutator args)
              base (if stamped? ctx c)]
          (cond-> base
            (seq warnings) (update :engine/warnings (fnil into []) warnings)))))
    :vis/silent))

(defn build-sci-bindings
  "Return `{'symbol bare-fn}` for every engine mutator. The model writes
   `(spec-set! :K {…})` or `(task-set! :K {:status :done :proof \"…\"})`
   directly inside a fence; we route the call through `apply-and-record!`
   against the single ctx-atom.

   All mutators return `:vis/silent` — engine mutations are 'effect-only',
   visible on next render but quiet in the form echo.

   D12: no `satisfy-hint!` binding. Hook-task satisfaction goes through
   the standard `task-set!` mutator with `{:status :done :proof \"…\"}`."
  [env]
  {'spec-set!     (fn spec-set!     [k partial]            (apply-and-record! env :spec-set!     [k partial]))
   'task-set!     (fn task-set!     [k partial]            (apply-and-record! env :task-set!     [k partial]))
   'fact-set!     (fn fact-set!     [k partial]            (apply-and-record! env :fact-set!     [k partial]))
   'req-add!      (fn req-add!      [spec-k req]           (apply-and-record! env :req-add!      [spec-k req]))
   'req-update!   (fn req-update!   [spec-k rid partial]   (apply-and-record! env :req-update!   [spec-k rid partial]))
   'req-remove!   (fn req-remove!   [spec-k rid]           (apply-and-record! env :req-remove!   [spec-k rid]))
   'proof-add!    (fn proof-add!    [task-k spec-k proof]  (apply-and-record! env :proof-add!    [task-k spec-k proof]))
   'proof-remove! (fn proof-remove! [task-k spec-k rid]    (apply-and-record! env :proof-remove! [task-k spec-k rid]))})

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

(defn reconcile-done-hook-tasks!
  "Run `eng/reconcile-done-hook-tasks` against the live ctx atom; append
   any reverted-task warnings to `:engine/warnings`. Called at end-of-
   iter (after advance-iter has pinned the trailer + bumped the cursor)
   so the validator sees the full form-results map.

   `form-results-map` is `{scope-string envelope}` for every scope in the
   trailer including the iter that just landed.

   No-op when ctx-atom is missing (defensive guard for tests that wire a
   partial env)."
  [{:keys [ctx-atom] :as env} form-results-map]
  (if-not ctx-atom
    (do (tel/log! {:level :warn :id ::reconcile-no-ctx-atom}
          "reconcile-done-hook-tasks! called without :ctx-atom on env")
      nil)
    (let [start-ms (System/currentTimeMillis)
          cursor   (cursor-snapshot env)
          warns-acc (atom [])]
      (swap! ctx-atom
        (fn [c]
          (let [c+cur (assoc c :session/scope cursor)
                {:keys [ctx warnings]} (eng/reconcile-done-hook-tasks c+cur form-results-map)]
            (reset! warns-acc (vec warnings))
            (cond-> ctx
              (seq warnings) (update :engine/warnings (fnil into []) warnings)))))
      (tel/log! {:level :info :id ::reconcile-done-hook-tasks
                 :data {:cursor cursor
                        :form-result-scopes (vec (sort (keys (or form-results-map {}))))
                        :warnings @warns-acc
                        :duration-ms (- (System/currentTimeMillis) start-ms)}}
        "reconcile-done-hook-tasks completed"))))

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
   renderer + `derive-warnings`."
  [env]
  (when-let [a (:ctx-atom env)]
    (stamp-cursor env @a)))

;; =============================================================================
;; Introspect verbs — model-facing SCI bindings over engine history helpers
;; =============================================================================

(defn- with-live-history
  "Build the {turn → ctx} history map used by engine introspect helpers.
   Combines:
     - the LIVE ctx-atom value (assigned to the env's :session/turn so the
       current in-progress turn is reachable via `introspect-ctx-at` and the
       per-key introspect verbs)
     - the persisted snapshots loaded via the provided `history-loader`
   The live value lands LAST (highest turn) so per-key lookups walking
   reverse find it first."
  [env history-loader]
  (let [live  (some-> (:ctx-atom env) deref)
        live-turn (or (:turn (cursor-snapshot env)) (:session/turn live) 1)
        loaded (history-loader)
        history (into {} loaded)]
    (cond-> history
      live (assoc live-turn live))))

(defn build-introspect-bindings
  "Return `{'symbol bare-fn}` for the introspect-* verbs the model calls.
   `history-loader` is a 0-arity thunk that returns the persisted history
   vec (pairs of `[turn-n ctx]`). The loop passes a thunk that calls
   `persistance/db-load-ctx-history` against the env's db + session.

   The live ctx-atom is folded in on every call so introspection on the
   IN-PROGRESS turn also works (otherwise the model could not query its
   own just-defined specs from the next iter)."
  [env history-loader]
  (let [history #(with-live-history env history-loader)]
    {'introspect-spec     (fn introspect-spec     [k]   (eng/introspect-spec (history) k))
     'introspect-task     (fn introspect-task     [k]   (eng/introspect-task (history) k))
     'introspect-fact     (fn introspect-fact     [k]   (eng/introspect-fact (history) k))
     'introspect-archived (fn introspect-archived [kind] (eng/introspect-archived (history) kind))
     'introspect-ctx-at   (fn introspect-ctx-at   [turn-key]
                            (let [t (cond
                                      (string? turn-key)
                                      (when-let [[_ n] (re-matches #"^t([1-9][0-9]*)$" turn-key)]
                                        (parse-long n))
                                      (number? turn-key) (long turn-key)
                                      :else nil)]
                              (when t (eng/introspect-ctx-at (history) t))))}))

;; =============================================================================
;; Trailer → form-results map for engine renderer
;; =============================================================================

(defn trailer->form-results
  "Flatten every trailer pin's :forms vec into a `{scope envelope}` map.
   Used at render time so engine's `classify-scope` and validator-fn pass
   can look up per-form `:result` / `:error` for any scope the model
   references on a task proof."
  [trailer]
  (into {}
    (for [entry trailer
          :when (some? (:forms entry))
          form  (:forms entry)
          :when (and (some? form) (some? (:scope form)))]
      [(:scope form) form])))
