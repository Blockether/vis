(ns com.blockether.vis.loop.runtime.conversation.environment.query.persistence
  "Query-level persistence boundary.

   Every function validates its input via unified specs.
   Short names — the namespace IS the context."
  (:require
   [com.blockether.vis.loop.runtime.shared :as shared]
   [com.blockether.vis.loop.storage.db :as rlm-db]))

(defn store!
  "Persist a new :query entity. Returns query-id [:id uuid].

   `env`   — RLM env (validated against ::shared/env).
   `query` — non-blank string."
  [env query]
  (shared/validate! ::shared/env env)
  (shared/validate! ::shared/non-blank-string query)
  (rlm-db/store-query! (:db-info env)
    {:parent-conversation-id (:conversation-id env)
     :parent-iteration-id    (:parent-iteration-id env)
     :query                  query
     :status                 :running}))

(defn update!
  "Update a :query entity with final results.

   `env`       — RLM env.
   `query-id` — [:id uuid] from store!.
   `result`    — iteration-loop result map."
  [env query-id result]
  (shared/validate! ::shared/entity-id query-id)
  (shared/validate! ::shared/query-result result)
  (rlm-db/update-query! (:db-info env) query-id
    {:answer      (:answer result)
     :iterations  (or (:iterations result) 0)
     :duration-ms (or (:duration-ms result) 0)
     :status      (or (:status result) :unknown)
     :tokens      (:tokens result)
     :cost        (:cost result)}))
