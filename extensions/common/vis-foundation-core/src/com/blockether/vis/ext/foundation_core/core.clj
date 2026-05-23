(ns com.blockether.vis.ext.foundation-core.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.editing.core :as editing]
   [com.blockether.vis.ext.foundation-core.environment.core :as environment]
   [com.blockether.vis.ext.foundation-core.environment.digest :as env-digest]
   [com.blockether.vis.ext.foundation-core.introspection :as introspection]
   [com.blockether.vis.ext.foundation-core.hints :as hints]
   [com.blockether.vis.ext.foundation-core.workspace-ctx :as workspace-ctx]
   [com.blockether.vis.ext.foundation-core.workspace-slashes :as workspace-slashes]
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

;; Every `v/*` symbol carries its `:tag :observation | :mutation`
;; INLINE on the (vis/symbol ...) opts map; register-extension! walks
;; the symbol vec and auto-populates the op registry. The old
;; flat (doseq [[op tag] ...] (vis/register-op! ...)) table retired.

(defn- lazy-doctor-fn
  [env]
  (call-resolved! 'com.blockether.vis.ext.foundation-core.doctor/doctor-fn env))

(defn- session-workspace-block
  "Resolve the env's pinned workspace + session-state and render the
   canonical `:session/workspace` CTX block. Returns nil when there
   is no DB / no workspace pin yet (engine seeds `:vcs/kind :none`
   from `empty-ctx` in that case)."
  [env]
  (when-let [db (:db-info env)]
    (when-let [ws-id (or (:workspace/id env) (some-> env :workspace :id))]
      (when-let [pair (workspace/workspace-with-session db ws-id)]
        (workspace-ctx/render-block pair)))))

(defn- combined-ctx
  "Foundation-core's single `:ext/ctx` fn. Merges:
     - the slim auto-pin `:session/env` digest (host / project /
       extensions) so the model sees cwd, OS, primary language, and
       active aliases without calling `v/snapshot`.
     - the legacy `(:project ctx)` block (fat snapshot + agents.md
       guidance + extension warnings), kept for deep-dive callers.
     - the workspace block under `:session/workspace`.
   Other extensions extend `:session/env` via their own `:ext/ctx`
   returning `{:session/env {their-key {…}}}`; the engine deep-merges
   contributions."
  [env]
  (let [project   (environment/environment-ctx env)
        digest    (try (env-digest/digest env) (catch Throwable _ nil))
        ws-block  (session-workspace-block env)]
    (cond-> (or project {})
      (seq digest) (assoc :session/env digest)
      ws-block (assoc :session/workspace ws-block))))

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-core"
     :ext/description    "Foundation `v/` kernel: session-state/session-report, file I/O (cat/ls/rg/patch/copy/move/delete/exists?), SCI symbol introspection (engine-symbol-{documentation,source-code,metadata,apropos}), env snapshot + project guidance (snapshot/repositories/git/languages/monorepo/main-agent-instructions). Answers are plain markdown strings — no DSL."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/sci            {:ext.sci/alias 'v
                          :ext.sci/symbols (vec (concat introspection/all-symbols
                                                  (editing/available-editing-symbols)
                                                  environment/environment-symbols))}
     :ext/kind           "foundation"
     :ext/hooks          hints/hooks
     :ext/slash-commands workspace-slashes/specs
     :ext/ctx            combined-ctx
     :ext/prompt         combined-prompt
     :ext/doctor-fn      lazy-doctor-fn}))

(vis/register-extension! vis-extension)
