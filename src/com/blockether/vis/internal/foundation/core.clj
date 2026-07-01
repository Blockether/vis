(ns com.blockether.vis.internal.foundation.core
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.doctor :as doctor]
   [com.blockether.vis.internal.foundation.editing.core :as editing]
   [com.blockether.vis.internal.foundation.environment.core :as environment]
   [com.blockether.vis.internal.foundation.introspection :as introspection]
   [com.blockether.vis.internal.foundation.language-surface :as language-surface]
   [com.blockether.vis.internal.foundation.workspace-ctx :as workspace-ctx]
   [com.blockether.vis.internal.foundation.workspace-slashes :as workspace-slashes]
   [com.blockether.vis.internal.workspace :as workspace]))

(defn- combined-prompt
  "Stitch foundation-owned tool strategy prompt text. Structured runtime
   and project guidance flow through `ctx`, not prompt labels."
  [env]
  (str/join "\n\n"
            (remove str/blank?
                    [(environment/environment-prompt env)
                     introspection/introspection-prompt
                     (language-surface/prompt env)            ; nil when no language pack is active
                     (editing/available-editing-prompt)])))

;; Every foundation symbol carries its `:tag :observation | :mutation`
;; INLINE on the (vis/symbol ...) opts map; register-extension! walks
;; the symbol vec and auto-populates the op registry.

(defn- lazy-doctor-fn
  [env]
  ;; Call doctor/doctor-fn directly via a build-time :require. Resolving it at
  ;; runtime (requiring-resolve) triggers a namespace load — i.e. defining a
  ;; class at runtime — which GraalVM native-image forbids ("Classes cannot be
  ;; defined at runtime"). doctor does not depend on this ns, so there is no
  ;; load cycle. See native-image notes in AGENTS.md.
  (doctor/doctor-fn env))

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
  "Foundation-core's single `:ext/ctx-fn` fn. Contributes the workspace
   block under `:session/workspace`.

   The slim auto-pin `:session/env` digest (host / project / extensions)
   moved to `internal.env-digest` — it's core functionality, not
   extension-owned. Workspace/VCS truth lives in `:session/workspace`.
   The old redundant `(:project ctx)` contribution is gone; slim digest
   covers it."
  [env]
  (let [ws-block (session-workspace-block env)
        ;; Recomputed EVERY turn from active-extensions, so the model sees a
        ;; language pack's verbs (repl_eval/test/format) the turn it activates.
        lang-tools (language-surface/capability-data env)]
    (cond-> {}
      ws-block   (assoc :session/workspace ws-block)
      lang-tools (assoc :session/language-tools lang-tools))))

(def vis-extension
  (vis/extension
   {:ext/name           "foundation-core"
    :ext/description    "Foundation kernel (bare Python functions): session_state/session_report_md/sessions, language facade (format_code/lint_code/run_tests/repl_eval/repl_start/repl_stop), file I/O (cat/find/rg/ls/patch/write/copy/move/delete/delete_if_exists/exists/is_exists), CTX workspace/VCS, project shape (repositories/languages/monorepo), and main_agent_instructions. Sandbox symbol introspection is an engine system call (doc / apropos), not a tool. Answers are plain markdown strings — no DSL."
    :ext/version        "0.7.0"
    :ext/author         "Blockether"
    :ext/owner          "vis"
    :ext/license        "Apache-2.0"
     ;; BUILT-IN: foundation is the mandatory kernel promoted into core, so
     ;; its symbols bind BARE into the sandbox ns (cat/find/rg/patch …) right next
     ;; to the engine verb `done` — NO `v/` alias. `:builtin?`
     ;; routes the binding through `extension/builtin-sandbox-bindings` instead
     ;; of the aliased-namespace path third-party extensions use.
    :ext/engine            {:ext.engine/builtin? true
                            :ext.engine/symbols (vec (concat introspection/all-symbols
                                                             language-surface/symbols
                                                             (editing/available-editing-symbols)
                                                             environment/environment-symbols))}
    :ext/kind           "foundation"
    :ext/slash-commands workspace-slashes/specs
    :ext/ctx-fn            combined-ctx
    :ext/prompt-fn         combined-prompt
    :ext/doctor-fn      lazy-doctor-fn}))

(vis/register-extension! vis-extension)
