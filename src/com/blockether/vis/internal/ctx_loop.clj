(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for context management.

   The loop keeps a per-session `:ctx-atom` for stable model-facing context
   and a separate `:turn-state-atom` for live turn/iteration/form counters.
   This namespace stamps the cursor, enriches context with env/resources/routing,
   and renders the standing context block."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.env-digest :as env-digest]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.resources :as resources]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Atom and constructor — ONE atom carries the entire engine state
;; =============================================================================

(defn make-ctx-atom
  "Initialize the CTX atom for a session. Uses the canonical empty scaffold.
   The scaffold carries an empty `\"engine_warnings\"` vec so every swap!
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
  (atom {:turn-position nil
         :iteration nil
         :form-idx nil
         :iteration-id nil
         :session-turn-id nil
         :user-request nil
         ;; This iteration's final answer ({:value :position}, reset each
         ;; iteration) and the sticky best answer across the turn — folded in
         ;; here instead of two separate atoms.
         :answer nil
         :best-answer nil}))

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
  (or (some-> (:turn-state-atom env)
              deref)
      {}))

(defn- normalize-iteration
  "Iteration field accepts a number, a {:position N} map, or nil. Returns N."
  [v]
  (cond (map? v) (or (:position v) 1)
        (number? v) v
        :else 1))

(defn synthesize-scope
  "Build the current form scope `tN/iM/fK` from `:turn-state-atom`.
   Defaults each field to 1 (form-idx defaults to 0 → next-form 1) so
   the helper is safe to call before the loop has initialised the
   atom (e.g. early hooks)."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    (str "t" (or turn-position 1)
         "/i" (normalize-iteration iteration)
         "/f" (inc (long (or form-idx 0))))))

(defn cursor-snapshot
  "Build a `\"session_scope\"` map from `:turn-state-atom`. Mirrors the
   engine's `{\"turn\" \"iter\" \"next_form\"}` shape (STRING keys — the ctx
   crosses the Python boundary) used by `classify-scope` and the renderer."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    {"turn" (or turn-position 1)
     "iter" (normalize-iteration iteration)
     "next_form" (inc (long (or form-idx 0)))}))

;; =============================================================================
;; Per-iter helpers used by the loop
;; =============================================================================

(defn drain-warnings!
  "Compatibility shim for the former warning drain. Always returns [] so the
   renderer's between-iters call site stays valid."
  [_env]
  [])

(defn finalize-turn!
  "Side-effecting turn finalizer; compaction is the standalone `session_fold` verb.

   Returns the intent map plus warnings."
  [{:keys [ctx-atom] :as env} {:keys [answer user-request turn-summary]}]
  (let
    [cursor
     (cursor-snapshot env)

     scope
     (synthesize-scope env)]

    (when ctx-atom
      (swap! ctx-atom (fn [c]
                        (let
                          [c+cur
                           (assoc c "session_scope" cursor)

                           {ctx' :ctx}
                           (eng/finalize-turn c+cur
                                              scope
                                              {:answer answer
                                               :user-request user-request
                                               :turn-summary turn-summary})]

                          (dissoc ctx' "session_scope"))))
      (tel/log! {:level :info
                 :id ::finalize-turn
                 :data {:answer-present? (not (clojure.string/blank? (str answer)))}}
                "finalize-turn completed"))
    {:answer answer :blocked? false :warnings []}))

(defn stamp-cursor
  "Return a ctx map with both `\"session_turn\"` and `\"session_scope\"` synced
   from the loop's running counters. Render path + every engine derivation
   call goes through this so the model never sees a stale top-level
   `\"session_turn\"` (e.g. after a resume that loaded turn N's snapshot but
   the current loop is on turn N+1)."
  [env ctx]
  (let [cursor (cursor-snapshot env)]
    (-> ctx
        (assoc "session_turn" (get cursor "turn"))
        (assoc "session_scope" cursor))))

(defn current-ctx
  "Deref the CTX atom with both `:session/turn` and `:session/scope`
   stamped from the loop counters. This is the shape passed to the
   renderer + `derive-warnings`. Returns nil when ctx-atom is missing
   on env (defensive for partial test envs)."
  [{:keys [ctx-atom] :as env}]
  (when ctx-atom (stamp-cursor env @ctx-atom)))

(defn enrich-ctx
  "Merge extension ctx contributions + live env/resource/routing enrichments
   into the raw ctx-atom value, producing the model-facing shape.

   THE single enrichment path — BOTH the rendered `<context>` TEXT
   (`render-block!`) and the live `session` dict bound in the sandbox
   (`session-snapshot`) derive from this. Keeping it in ONE place is
   load-bearing: when the two paths computed different shapes, the model saw
   keys in the text that KeyError'd in code. That is exactly how
   `session[\"workspace\"]` (and `workspace[\"filesystem_roots\"]`) went missing
   from the bound dict — the ext-contributed `\"session_workspace\"` block was
   merged into the TEXT but not into the binding. Merge ALL ext-ctx so no
   future contributed key can drift the same way.

   Recomputed each call so transient extension/env/resource/routing state stays
   fresh without pushing it back into `ctx-atom`."
  [env ctx]
  (let
    [active-exts
     (try (prompt/active-extensions env) (catch Throwable _ nil))

     ext-ctx
     (try (extension/ctx-contributions env active-exts)
          (catch Throwable t
            (tel/log! {:level :warn :id ::ctx-contributions-failed :data {:error (ex-message t)}})
            {}))

     env-block
     (try (env-digest/deep-merge (env-digest/base-digest env) (get ext-ctx "session_env"))
          (catch Throwable t
            (tel/log! {:level :warn :id ::env-digest-failed :data {:error (ex-message t)}})
            nil))

     ;; Session-scoped live resources — same registry the footer reads, so
     ;; `session["resources"]` and the footer can never disagree.
     rsrc
     (try (resources/list-resources (:session-id env)) (catch Throwable _ nil))

     rsrc-view
     (resources/model-view rsrc
                           {:root (or (:workspace/root env)
                                      (get-in ext-ctx ["session_workspace" "root"]))
                            :languages (keys (get ext-ctx "session_language_tools"))})

     access-view
     (try (when-let [f (:access-view-fn env)]
            (f))
          (catch Throwable t
            (tel/log! {:level :warn :id ::access-view-failed :data {:error (ex-message t)}})
            nil))]

    (cond-> (env-digest/deep-merge ctx (dissoc ext-ctx "session_env"))
      (seq env-block)
      (assoc "session_env" env-block)

      (seq rsrc-view)
      (assoc "session_resources" rsrc-view)

      (seq access-view)
      (assoc "session_access" access-view)

      ;; current model + available models, so the agent can route a sub_loop
      ;; child by cost (read-only). `:routing env` is loop-internal; its VALUE
      ;; is built string-keyed at the loop because it crosses the boundary.
      (seq (:routing env))
      (assoc "session_routing" (:routing env)))))

(defn session-snapshot
  "Read-only data mirror of the Python `session` dict bound in the sandbox.

   Built to MATCH the rendered shape, NOT the raw atom: stamps the live cursor,
   runs the shared `enrich-ctx` (so workspace/env/resources/routing are present
   exactly as in the rendered `<context>` TEXT), then delegates to the canonical
   `eng/session-view` projection. Returns nil when ctx-atom is absent."
  [env]
  (when-let [ctx (current-ctx env)]
    (eng/session-view (enrich-ctx env ctx))))

(defn render-block!
  "Build the standing `<context>` block for the next user message. Pure data
   flow, with extension/env/resource/routing enrichments recomputed each render
   so transient state stays fresh without pushing it back into `ctx-atom`."
  [env renderer-fn]
  (when-let [ctx (current-ctx env)]
    ;; SAME enrichment the live `session` binding gets (enrich-ctx), so the
    ;; rendered TEXT and the bound dict are one shape by construction.
    ;; Tool results reach the model through append-only message history,
    ;; not through mutable context.
    (renderer-fn {:ctx (enrich-ctx env ctx) :warnings []})))

;; =============================================================================
;; rewind / lens / find — model-facing recovery bindings
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; All DB-backed against `session_turn_iteration.forms`.
;; =============================================================================
