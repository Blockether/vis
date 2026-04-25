(ns com.blockether.vis.core
  "Public API facade for the Vis RLM (Recursive Language Model).

   Sections:
   - Unified entrypoint
   - Environment lifecycle
   - Query execution"
  (:require
   [com.blockether.vis.loop.core :as loop-core]
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
  "Default iteration budget per query. The LLM can extend it
   at runtime via (request-more-iterations n) up to MAX_ITERATION_CAP."
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

;; =============================================================================
;; Query execution
;; =============================================================================

(def query!
  "Runs a query against an RLM environment using iterative LLM code evaluation.

   `environment` — RLM environment from `create-environment`.
   `messages`    — Vector of message maps, e.g. [(llm/user \"...\")].
   `opts`        — Optional map. See query.core/query! for all opts.

   Returns map with :answer, :trace, :iterations, :duration-ms,
   :tokens, :cost, :confidence, :sources, :reasoning.
   On failure also :status and :status-id."
  query/query!)
