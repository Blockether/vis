(ns com.blockether.vis.core
  "Public API facade for the Vis RLM (Recursive Language Model).

   Sections:
   - Unified entrypoint
   - Environment lifecycle
   - Query execution"
  (:require
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.loop.runtime.conversation.environment.extension :as ext]
   [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query]
   [com.blockether.vis.persistance.spec :as rlm-spec]))

;; =============================================================================
;; Unified entrypoint
;; =============================================================================

(defn -main
  "Unified entrypoint for all Vis channels.

   Dispatches to the appropriate channel based on the first argument:

     vis chat              TUI chat interface (default when no args)
     vis run <prompt>      One-shot CLI query
     vis web [port]        Start web server (default port 3000)
     vis telegram          Start Telegram bot
     vis help              Show usage

   Examples:
     (com.blockether.vis.core/-main)                        ;; → TUI
     (com.blockether.vis.core/-main \"run\" \"What is 2+2?\")  ;; → CLI
     (com.blockether.vis.core/-main \"web\" \"8080\")          ;; → Web on port 8080
     (com.blockether.vis.core/-main \"telegram\")             ;; → Telegram bot"
  [& args]
  ;; Require at call time to avoid loading all channels eagerly
  (require 'com.blockether.vis.channels.cli)
  (apply (resolve 'com.blockether.vis.channels.cli/-main) args))

;; =============================================================================
;; Constants
;; =============================================================================

(def MAX_ITERATIONS
  "Default iteration budget per query (4). System nudges tell the LLM
   how to extend on demand when needed. No cap."
  rlm-spec/MAX_ITERATIONS)

;; =============================================================================
;; Environment lifecycle
;; =============================================================================

(defn create-environment
  "Creates an RLM environment for querying.

   `router` — LLM router from `config/make-router`.
   `opts`   — Map with :db (path or :temp), :conversation (nil | :latest | uuid).

   Returns an environment map with :db-info, :sci-ctx, :router, etc."
  [router opts]
  (loop-core/create-environment router opts))

(defn dispose-environment!
  "Disposes an RLM environment and releases resources.
   The shared SQLite DataSource stays open for sibling envs."
  [environment]
  (loop-core/dispose-environment! environment))

(def register-extension!
  "Register a validated extension into a live environment. Re-exported from loop.core."
  loop-core/register-extension!)

(def active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn`
   returns truthy for `environment`. Call this ONCE per query and thread
   the resulting vec through every consumer (`assemble-system-prompt`,
   per-iteration nudge collectors). See `loop.core/active-extensions`."
  loop-core/active-extensions)

(def assemble-system-prompt
  "Build the full system prompt: core instructions + ACTIVE extension prompts.
   REQUIRES `:active-extensions` (vec from `active-extensions`). See
   `loop.core/assemble-system-prompt` for the contract."
  loop-core/assemble-system-prompt)

;; =============================================================================
;; Extension helpers
;; =============================================================================

(def extension
  "Build and validate an extension spec. Re-exported from environment.extension."
  ext/extension)

(def symbol
  "Build a function symbol entry for an extension. Re-exported from environment.extension."
  ext/symbol)

(def value
  "Build a value symbol entry for an extension. Re-exported from environment.extension."
  ext/value)

(def render-extension-prompt
  "Render canonical extension prompt text from symbol docstrings + arglists.
   Kept as a compatibility alias for `preview-extension-prompt`."
  ext/render-prompt)

(def preview-extension-prompt
  "Preview the canonical extension prompt text exactly as loop-side prompt
   assembly renders its symbol-derived block."
  ext/render-prompt)

(def register-global!
  "Register an extension in the process-level global registry."
  ext/register-global!)

(def registered-extensions
  "Return all globally registered extensions."
  ext/registered-extensions)

(def discover-extensions!
  "Discover extension namespaces from META-INF/vis/extensions.edn on the classpath."
  ext/discover-extensions!)

(def load-extension!
  "Require an extension namespace and return the registered extension."
  ext/load-extension!)

(def reload-extension!
  "Reload an extension namespace and optionally hot-swap it into live environments."
  ext/reload-extension!)

;; =============================================================================
;; Query execution
;; =============================================================================

(def query!
  "Runs a query against an RLM environment using iterative LLM code evaluation.

   `environment` — RLM environment from `create-environment`.
   `messages`    — Vector of message maps, e.g. [(llm/user \"...\")].
   `opts`        — Optional map. See query.core/query! for all opts.

   Returns map with :answer, :trace, :iterations, :duration-ms,
   :tokens, :cost, :confidence, :reasoning.
   On failure also :status and :status-id."
  query/query!)
