(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for the new CTX engine.

   The pure engine (`ctx-engine`) takes data, returns data. Real-world wiring
   needs side effects: a mutable CTX atom that lives across iters within a
   turn, a scope cursor derived from the loop's running counters, and SCI
   symbol bindings the model can call directly from inside a fence.

   This namespace is the thin adapter that ties the engine to the loop. It
   owns three things:

     1. **A `:ctx-atom`** on the env map, initialised to `(eng/empty-ctx …)`.
        Mutators in the SCI sandbox swap! this atom. Read at render time;
        Nippy-snapshotted to `session_turn_state.ctx` at `(done …)` time.

     2. **A `:ctx-warnings-atom`** that collects per-mutation warnings during
        an iter. The renderer drains it (or merges with derive-warnings) when
        producing the next user message.

     3. **`build-sci-bindings`** — a map of `{'symbol sci-fn}` ready to be
        merged into the loop's `env-bindings`. Each mutator:
        - Synthesises the current form scope from the loop atoms
        - Calls `eng/apply-mutator`
        - Resets the ctx atom on success
        - Appends warnings to the warnings atom
        - Returns `:vis/silent` so the form result isn't echoed

   The integration is intentionally narrow: we don't touch eval loop,
   trailer pinning, persistence, or rendering here. Those layers consume
   the atoms via their own wiring."
  (:require [com.blockether.vis.internal.ctx-engine :as eng]))

;; =============================================================================
;; Atoms and constructors
;; =============================================================================

(defn make-ctx-atom
  "Initialize the CTX atom for a session. Uses the canonical empty scaffold."
  ([] (atom (eng/empty-ctx)))
  ([session-id] (atom (eng/empty-ctx session-id))))

(defn make-warnings-atom
  "Per-iter mutator-time warning sink. Each entry is a `derive-warnings`
   shape: `{:code :anchor :message}`. The loop drains this between iters
   when emitting the user message; the engine's `derive-warnings` is the
   render-time companion (covers state invariants), this atom covers
   per-call mutation warnings (collision rejects, hard-rejects, etc)."
  []
  (atom []))

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

(defn- apply-and-record!
  "Call the engine mutator, swap! the ctx atom on success, accumulate
   warnings. Returns `:vis/silent` so the form result is hidden from the
   model's eval echo (the mutation effect is visible on next render)."
  [{:keys [ctx-atom ctx-warnings-atom] :as env} mutator args]
  (let [scope  (synthesize-scope env)
        ;; Engine sees a ctx with the cursor stamped in so `classify-scope`
        ;; and validator hooks have current coordinates. We don't persist
        ;; the cursor back into the atom directly — the renderer stamps
        ;; it fresh on each render.
        ctx    (assoc @ctx-atom :session/scope (cursor-snapshot env))
        {:keys [ctx warnings stamped?]} (eng/apply-mutator ctx scope mutator args)]
    (when stamped? (reset! ctx-atom ctx))
    (when (seq warnings)
      (swap! ctx-warnings-atom into warnings))
    :vis/silent))

(defn build-sci-bindings
  "Return `{'symbol bare-fn}` for every engine mutator. Caller (loop env
   builder) merges this into the SCI env so the model can write
   `(spec-set! :K {…})` directly from a fence.

   All mutators return `:vis/silent` — engine mutations are 'effect-only',
   visible on next render but quiet in the form echo."
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
  "Atomically read and clear the warnings atom. Called by the renderer
   between iters."
  [env]
  (let [a (:ctx-warnings-atom env)]
    (when a
      (let [ws @a]
        (reset! a [])
        ws))))

(defn current-ctx
  "Deref the CTX atom with the engine cursor stamped in. This is the
   shape passed to the renderer + `derive-warnings`."
  [env]
  (when-let [a (:ctx-atom env)]
    (assoc @a :session/scope (cursor-snapshot env))))
