(ns com.blockether.vis.internal.foundation.core
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.doctor :as doctor]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.vis.internal.foundation.environment.core :as environment]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.internal.foundation.workspace-ctx :as workspace-ctx]
            [com.blockether.vis.internal.foundation.workspace-slashes :as workspace-slashes]
            [com.blockether.vis.internal.foundation.session-slashes :as session-slashes]
            [com.blockether.vis.internal.workspace :as workspace]))

(defn- combined-prompt
  "Render only the dynamic language capability matrix. Native descriptions own
   tool routing; CORE owns cross-tool policy."
  [env]
  (or (language-surface/prompt env) ""))

;; Every foundation symbol carries its `:tag :observation | :mutation`
;; INLINE on the (vis/symbol ...) opts map; register-extension! walks
;; the symbol vec and auto-populates the op registry.

(defn- lazy-doctor-fn
  [env]
  ;; Call doctor/doctor-fn directly via a build-time :require. Resolving it at
  ;; runtime (requiring-resolve) triggers a namespace load — i.e. defining a
  ;; class at runtime — which GraalVM native-image forbids ("Classes cannot be
  ;; defined at runtime"). doctor does not depend on this ns, so there is no
  ;; load cycle; native-image cannot define that class at runtime.
  (doctor/doctor-fn env))

(defn- fallback-workspace
  [env]
  {:root (or (workspace/workspace-root env) (workspace/normalize-root (workspace/cwd)))})

(defn- session-workspace-block
  "Resolve the env's pinned workspace and render the canonical session workspace CTX block."
  [env]
  (let
    [db
     (:db-info env)

     ws-id
     (or (:workspace/id env)
         (some-> env
                 :workspace
                 :id))

     pair
     (when (and db ws-id) (workspace/workspace-with-session db ws-id))]

    (workspace-ctx/render-block (assoc (or pair {:workspace (fallback-workspace env)})
                                  :filesystem-roots (workspace/env-filesystem-roots env)))))

(defn- combined-ctx
  "Foundation-core's single `:ext/ctx-fn` fn. Contributes the workspace
   block under `\"session_workspace\"` (STRING-KEYED — crosses the Python
   boundary).

   The slim auto-pin `\"session_env\"` digest (host / project / extensions)
   moved to `internal.env-digest` — it's core functionality, not
   extension-owned. Workspace/VCS truth lives in `\"session_workspace\"`.
   The old redundant `(:project ctx)` contribution is gone; slim digest
   covers it."
  [env]
  (let
    [ws-block
     (session-workspace-block env)

     ;; Recomputed EVERY turn from active-extensions, so the model sees a
     ;; language pack's verbs (repl_eval/test/format) the turn it activates.
     lang-tools
     (language-surface/capability-data env)]

    (cond-> {}
      ws-block
      (assoc "session_workspace" ws-block)

      lang-tools
      (assoc "session_language_tools" lang-tools))))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-core"
     :ext/description
     "Foundation kernel: language facade; file editing; session workspace/VCS and project-shape helpers; `main_agent_instructions`. Session introspection lives in `foundation-introspection` behind its toggle; Vis' own documentation pages are corpus entries the engine verbs `apropos`/`doc` search and retrieve. Bare Python functions return plain Markdown."
     :ext/version "0.7.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     ;; BUILT-IN: foundation is the mandatory kernel promoted into core, so
     ;; its symbols bind BARE into the sandbox ns (cat/find/rg/patch …) right next
     ;; to the engine verb `done` — NO `v/` alias. `:builtin?`
     ;; routes the binding through `extension/builtin-sandbox-bindings` instead
     ;; of the aliased-namespace path third-party extensions use.
     :ext/engine {:ext.engine/builtin? true
                  :ext.engine/symbols (vec (concat language-surface/symbols
                                                   (editing/available-editing-symbols)
                                                   environment/environment-symbols))}
     :ext/kind "foundation"
     :ext/slash-commands (into workspace-slashes/specs session-slashes/specs)
     :ext/ctx-fn combined-ctx
     :ext/prompt-fn combined-prompt
     :ext/doctor-fn lazy-doctor-fn}))

(vis/register-extension! vis-extension)
