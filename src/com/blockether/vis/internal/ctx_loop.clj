(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for context management.

   The loop keeps a per-session `:ctx-atom` for stable model-facing context
   and a separate `:turn-state-atom` for live turn/iteration/form counters.
   This namespace stamps the cursor, enriches context with env/resources/routing,
   and renders the standing context block."
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
  "Return model-facing context mutator bindings.

   There are currently no context mutators left to bind — `done()` is bound by
   the loop's done handler. Kept as the named call site in case a future engine
   verb needs binding."
  [_env]
  {})

;; =============================================================================
;; Per-iter helpers used by the loop
;; =============================================================================

(defn drain-warnings!
  "Compatibility shim for the former warning drain. Always returns [] so the
   renderer's between-iters call site stays valid."
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
  "Read-only data mirror of the Python `context` dict bound in the sandbox.

   Built to MATCH the rendered shape, NOT the raw atom: this fn stamps the live
   cursor, attaches cheap env/resource/routing enrichments, then delegates to
   the canonical `eng/session-view` projection. Returns nil when ctx-atom is
   absent."
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
  "Build the standing `<context>` block for the next user message. Pure data
   flow, with extension/env/resource/routing enrichments recomputed each render
   so transient state stays fresh without pushing it back into `ctx-atom`."
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
      ;; Tool results reach the model through append-only message history,
      ;; not through mutable context.
      (renderer-fn {:ctx ctx* :warnings []}))))

;; =============================================================================
;; rewind / lens / find — model-facing recovery bindings
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; All DB-backed against `session_turn_iteration.forms`.
;; =============================================================================

