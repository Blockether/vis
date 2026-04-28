(ns com.blockether.vis-runtime.core
  "Public API facade for the Vis RLM (Recursive Language Model).

   Sections:
   - Environment lifecycle
   - Query execution
   - Runtime impl registration into vis-sdk

   NOTE: the extension contract (extension/symbol/value/register-extension!
   and friends) is NOT re-exported here. It lives in the
   `com.blockether.vis-sdk.core` namespace. Extensions should require
   that namespace directly.

   Loading this namespace also wires every iteration-runtime fn that
   vis-sdk fronts (the conversation API, the router rebuild hook, the
   svar wrappers) into vis-sdk's `runtime-impl` registry. After
   `(require 'com.blockether.vis-runtime.core)` the SDK fns
   `sdk/create-conversation!`, `sdk/send!`, `sdk/router-context-limit`,
   `sdk/parse-llm-response`, `sdk/user-message` work end-to-end with no
   `requiring-resolve` indirection."
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis-runtime.loop.core :as loop-core]
   [com.blockether.vis-runtime.loop.runtime.conversation.core :as conversations]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.query.core :as query]
   [com.blockether.vis-sdk.core :as sdk]))

;; =============================================================================
;; Constants
;; =============================================================================
;;
;; The CLI entrypoint lives in `com.blockether.vis-main.core`
;; (the `:vis` alias / `bin/vis` wrapper). vis-runtime is a library
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
;; namespace. They live in `com.blockether.vis-sdk.core` — require that
;; namespace directly when authoring an extension.
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

;; =============================================================================
;; Wire vis-sdk's runtime-impl registry
;; =============================================================================
;;
;; This block is the bridge that lets channels (which depend ONLY on
;; vis-sdk) reach the iteration-runtime-owned APIs without ever
;; importing vis-runtime. Every entry below maps a vis-sdk dispatch key
;; to the concrete fn that lives here in vis-runtime.
;;
;; Loaded as a side effect of requiring this ns. vis-main does that
;; at boot via its `:require` graph; SDK consumers that want to run
;; the iteration loop programmatically should also require this ns
;; (it's the public API facade, so they would anyway).
;; =============================================================================

(sdk/register-runtime-impl! :rebuild-router!         query/rebuild-router!)
(sdk/register-runtime-impl! :refresh-cached-routers! conversations/refresh-cached-routers!)
(sdk/register-runtime-impl! :user-message            svar-llm/user)
(sdk/register-runtime-impl! :router-context-limit    svar-router/context-limit)
(sdk/register-runtime-impl! :parse-llm-response      svar/str->data)
(sdk/register-runtime-impl! :conversation
  {:create!                          conversations/create!
   :by-id                            conversations/by-id
   :by-channel                       conversations/by-channel
   :for-telegram-chat!               conversations/for-telegram-chat!
   :set-title!                       conversations/set-title!
   :env-for                          conversations/env-for
   :effective-system-prompt          conversations/effective-system-prompt
   :send!                            conversations/send!
   :close!                           conversations/close!
   :delete!                          conversations/delete!
   :sweep-orphaned-running-queries!  conversations/sweep-orphaned-running-queries!
   :close-all!                       conversations/close-all!})
