(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.persistence
  "Iteration-level persistence boundary.

   Every function validates its input via unified specs.
   Short names — the namespace IS the context."
  (:require
   [com.blockether.vis.loop.runtime.shared :as shared]
   [com.blockether.vis.loop.storage.db :as rlm-db]))

(defn store!
  "Persist a single :iteration entity. Returns iter-id [:id uuid].

   `env`  — RLM env (must have :db-info).
   `opts` — validated against ::shared/iteration-store-opts."
  [env opts]
  (shared/validate! ::shared/iteration-store-opts opts)
  (rlm-db/store-iteration! (:db-info env) opts))
