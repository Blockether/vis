(ns com.blockether.vis.core
  "Public API facade for the Vis RLM (Recursive Language Model).

   Sections:
   - Environment lifecycle
   - Query execution

   NOTE: the extension contract (extension/symbol/value/register-global!
   and friends) is NOT re-exported here anymore. It lives in the
   standalone `com.blockether/vis-extension` library so extensions can
   depend on a tiny surface (telemere + clojure.spec only) without
   pulling the whole vis runtime. Extensions should require
   `com.blockether.vis.extension` directly."
  (:require
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query]))

;; =============================================================================
;; Constants
;; =============================================================================
;;
;; The CLI entrypoint lives in `com.blockether.vis.commandline.main`
;; (the `:vis` alias / `bin/vis` wrapper). vis-core is a library
;; namespace — not a `-main` host. Use `create-environment` /
;; `query!` / `dispose-environment!` from this namespace, or
;; `bin/vis <cmd>` from the shell.
;; =============================================================================

;; =============================================================================
;; Environment lifecycle
;; =============================================================================

(defn create-environment
  "Creates an RLM environment for querying.

   `router` — LLM router from `config/make-router`.
   `opts`   — Map with :db (path or :memory), :conversation (nil | :latest | uuid).

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
;; Query execution
;; =============================================================================
;;
;; Extension helpers (extension, symbol, value, register-global!,
;; registered-extensions, discover-extensions!, load-extension!,
;; reload-extension!, render-prompt) intentionally removed from this
;; namespace. They live in `com.blockether.vis.extension` now — require
;; that namespace directly when authoring an extension.
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
