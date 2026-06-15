(ns com.blockether.vis.internal.foundation.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.editing.core :as editing]
   [com.blockether.vis.internal.foundation.environment.core :as environment]
   [com.blockether.vis.internal.foundation.introspection :as introspection]
   [com.blockether.vis.internal.foundation.workspace-ctx :as workspace-ctx]
   [com.blockether.vis.internal.foundation.workspace-slashes :as workspace-slashes]
   [com.blockether.vis.internal.workspace :as workspace]))

(defn- combined-prompt
  "Stitch foundation-owned tool strategy prompt text. Structured runtime
   and project guidance flow through `ctx`, not prompt labels."
  [env]
  (str (environment/environment-prompt env)
    "\n\n"
    introspection/introspection-prompt
    "\n\n"
    (editing/available-editing-prompt)))

(defn- call-resolved!
  [sym & args]
  (apply (or (requiring-resolve sym)
           (throw (ex-info "Foundation helper did not resolve"
                    {:type :foundation/missing-helper
                     :symbol sym})))
    args))

;; Every foundation symbol carries its `:request-modes` capability set INLINE
;; on the (vis/symbol ...) opts map; register-extension! walks
;; the symbol vec and auto-populates the op registry. The old
;; flat (doseq [[op tag] ...] (vis/register-op! ...)) table retired.

(defn- lazy-doctor-fn
  [env]
  (call-resolved! 'com.blockether.vis.internal.foundation.doctor/doctor-fn env))

(defn- fallback-workspace
  [env]
  {:root (or (workspace/workspace-root env)
           (workspace/normalize-root (workspace/cwd)))})

(defn- session-workspace-block
  "Resolve the env's pinned workspace + session-state and render the
   canonical `:session/workspace` CTX block. If no DB / pin exists yet,
   render the current root as a real workspace; VCS detection inside
   `workspace-ctx/render-block` decides `:vcs/kind` (`:git`, `:none`, ...)."
  [env]
  (let [db    (:db-info env)
        ws-id (or (:workspace/id env) (some-> env :workspace :id))
        pair  (when (and db ws-id)
                (workspace/workspace-with-session db ws-id))]
    (workspace-ctx/render-block
      (or pair {:workspace (fallback-workspace env)}))))

(defn- combined-ctx
  "Foundation-core's single `:ext/ctx` fn. Contributes the workspace
   block under `:session/workspace`.

   The slim auto-pin `:session/env` digest (host / project / extensions)
   moved to `internal.env-digest` — it's core functionality, not
   extension-owned. Workspace/VCS truth lives in `:session/workspace`.
   The old redundant `(:project ctx)` contribution is gone; slim digest
   covers it."
  [env]
  (let [ws-block (session-workspace-block env)]
    (cond-> {}
      ws-block (assoc :session/workspace ws-block))))

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-core"
     :ext/description    "Foundation kernel (bare Python functions, no alias): session_state/session_report, file I/O (cat/ls/rg/patch/write/copy/move/delete/delete_if_exists/is_exists), CTX workspace/VCS, project shape (repositories/languages/monorepo), and main_agent_instructions. Sandbox symbol introspection is an engine system call (doc / apropos), not a tool. Answers are plain markdown strings — no DSL."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     ;; BUILT-IN: foundation is the mandatory kernel promoted into core, so
     ;; its symbols bind BARE into the sandbox ns (cat/ls/rg/patch …) right next
     ;; to the engine verbs (done/task-set!/recall) — NO `v/` alias. `:builtin?`
     ;; routes the binding through `extension/builtin-sandbox-bindings` instead
     ;; of the aliased-namespace path third-party extensions use.
     :ext/engine            {:ext.engine/builtin? true
                             :ext.engine/symbols (vec (concat introspection/all-symbols
                                                        (editing/available-editing-symbols)
                                                        environment/environment-symbols))}
     :ext/kind           "foundation"
     :ext/slash-commands workspace-slashes/specs
     :ext/ctx            combined-ctx
     :ext/prompt         combined-prompt
     :ext/doctor-fn      lazy-doctor-fn}))

(vis/register-extension! vis-extension)
