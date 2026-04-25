(ns com.blockether.vis.persistance.core
  "Persistence facade.

   RULE: callers use this namespace only; backend implementations stay internal."
  (:require [com.blockether.vis.persistance.sqlite.core :as sqlite]))

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(defn create-rlm-conn
  "Open a persistence connection from db-spec.

   db-spec forms:
     nil              — no DB (returns nil)
     :temp / :memory  — SQLite in-memory (ephemeral)
     \"path/to.db\"   — persistent SQLite file
     {:path p}        — persistent SQLite file
     {:datasource ds} — caller-owned DataSource (NOT closed on dispose)"
  [db-spec]
  (let [normalized (cond
                     (= :temp db-spec) :memory
                     (= :memory db-spec) :memory
                     (and (map? db-spec) (= :sqlite (:backend db-spec)))
                     (cond
                       (:datasource db-spec) {:datasource (:datasource db-spec)}
                       (:conn db-spec)       {:conn (:conn db-spec)}
                       (:path db-spec)       (:path db-spec)
                       :else db-spec)
                     :else db-spec)]
    (sqlite/open-store normalized)))

(defn dispose-rlm-conn!
  "Close a persistence connection."
  [store]
  (sqlite/close-store store))

;; =============================================================================
;; Delegated API — only functions that exist in sqlite/core.clj
;; =============================================================================
;;
;; Each fn here delegates to sqlite/core.clj. No magic macro, no ns-resolve.
;; If you need a new persistence fn, add it to sqlite/core.clj first,
;; then add a plain defn here.

;; --- Logging ---

(defn log! [db-info opts] (sqlite/log! db-info opts))

;; --- Conversation lifecycle ---

(defn store-conversation! [db-info opts] (sqlite/store-conversation! db-info opts))
(defn db-get-conversation [db-info ref] (sqlite/db-get-conversation db-info ref))
(defn db-resolve-conversation-id [db-info sel] (sqlite/db-resolve-conversation-id db-info sel))
(defn db-list-conversations [db-info channel] (sqlite/db-list-conversations db-info channel))
(defn db-find-conversation-by-external [db-info channel ext-id] (sqlite/db-find-conversation-by-external db-info channel ext-id))
(defn db-update-conversation-title! [db-info ref title] (sqlite/db-update-conversation-title! db-info ref title))
(defn delete-conversation-tree! [db-info id] (sqlite/delete-conversation-tree! db-info id))
(defn fork-conversation! [db-info conv-id opts] (sqlite/fork-conversation! db-info conv-id opts))

;; --- Query lifecycle ---

(defn store-query! [db-info opts] (sqlite/store-query! db-info opts))
(defn update-query! [db-info query-id opts] (sqlite/update-query! db-info query-id opts))
(defn db-list-queries-by-status [db-info status] (sqlite/db-list-queries-by-status db-info status))
(defn db-list-conversation-queries [db-info conv-ref] (sqlite/db-list-conversation-queries db-info conv-ref))
(defn retry-query! [db-info query-soul-id opts] (sqlite/retry-query! db-info query-soul-id opts))

;; --- Iteration lifecycle ---

(defn store-iteration!
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:query-id opts)
    (throw (ex-info "store-iteration! requires :query-id" {:opts (keys opts)})))
  (sqlite/store-iteration! db-info opts))
(defn db-list-query-iterations [db-info query-ref] (sqlite/db-list-query-iterations db-info query-ref))
(defn db-list-iteration-vars [db-info iter-ref] (sqlite/db-list-iteration-vars db-info iter-ref))

;; --- Var registry & history ---

(defn db-latest-var-registry
  ([db-info conv-ref] (sqlite/db-latest-var-registry db-info conv-ref))
  ([db-info conv-ref opts] (sqlite/db-latest-var-registry db-info conv-ref opts)))
(defn db-var-history [db-info conv-ref sym] (sqlite/db-var-history db-info conv-ref sym))
(defn db-query-history [db-info conv-ref] (sqlite/db-query-history db-info conv-ref))

;; --- Dependencies ---

(defn store-dependency! [db-info opts] (sqlite/store-dependency! db-info opts))
(defn db-list-dependencies [db-info conv-state-id] (sqlite/db-list-dependencies db-info conv-state-id))

;; --- Restore ---

(defn db-restore-expressions [db-info conv-id] (sqlite/db-restore-expressions db-info conv-id))


