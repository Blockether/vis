(ns com.blockether.vis.loop.storage.db
  "Public persistence contract over the SQL store.

   Soul/state pattern:
     conversation (soul) → conversation_state (state)  — forking via parent_state_id
     iteration_var (soul) → iteration_var_state (state) — versioned var mutations

   db-info map layout:
     {:datasource javax.sql.DataSource
      :backend    :sqlite
      :mode       :memory|:persistent|:external}"
  (:require
   [com.blockether.vis.loop.storage.sqlite.conversations :as conv]
   [com.blockether.vis.loop.storage.sqlite.core :as core]))

(def ->id core/->id)
(def ->kw core/->kw)
(def ->epoch-ms core/->epoch-ms)

;; -----------------------------------------------------------------------------
;; Connection lifecycle
;; -----------------------------------------------------------------------------

(def create-rlm-conn   core/open-store)
(def dispose-rlm-conn! core/close-store)

;; -----------------------------------------------------------------------------
;; Conversation soul + state
;; -----------------------------------------------------------------------------

(def store-conversation!               conv/store-conversation!)
(def db-get-conversation               conv/db-get-conversation)
(def db-resolve-conversation-id        conv/db-resolve-conversation-id)
(def db-list-conversations             conv/db-list-conversations)
(def db-find-conversation-by-external  conv/db-find-conversation-by-external)
(def db-update-conversation-title!     conv/db-update-conversation-title!)
(def fork-conversation!                conv/fork-conversation!)

;; -----------------------------------------------------------------------------
;; Query
;; -----------------------------------------------------------------------------

(def store-query!    conv/store-query!)
(def update-query!   conv/update-query!)

;; -----------------------------------------------------------------------------
;; Iteration + Execution + Vars
;; -----------------------------------------------------------------------------

(def store-iteration!         conv/store-iteration!)
(def db-list-executions       conv/db-list-executions)
(def db-list-iteration-vars   conv/db-list-iteration-vars)
(def db-list-var-states       conv/db-list-var-states)
(def db-latest-var-registry   conv/db-latest-var-registry)

;; -----------------------------------------------------------------------------
;; List children
;; -----------------------------------------------------------------------------

(def db-list-conversation-queries  conv/db-list-conversation-queries)
(def db-list-queries-by-status     conv/db-list-queries-by-status)
(def db-list-query-iterations      conv/db-list-query-iterations)

;; -----------------------------------------------------------------------------
;; Delete
;; -----------------------------------------------------------------------------

(def delete-conversation-tree!     core/delete-conversation-tree!)

;; -----------------------------------------------------------------------------
;; Derived views
;; -----------------------------------------------------------------------------

(defn db-query-history
  "Ordered query history for a conversation with compact summaries."
  [db-info conversation-id]
  (let [queries (db-list-conversation-queries db-info conversation-id)]
    (mapv (fn [idx query]
            (let [qref      [:id (:id query)]
                  iter-count (count (db-list-query-iterations db-info qref))
                  answer-preview (let [raw (or (:answer query) "")]
                                   (subs raw 0 (min (count raw) 160)))]
              {:query-pos      idx
               :query-id       (:id query)
               :created-at     (:created-at query)
               :query          (:text query)
               :status         (:status query)
               :iterations     iter-count
               :answer-preview answer-preview}))
      (range)
      queries)))

(defn db-var-history
  "Full version history for a named var, oldest first."
  [db-info conversation-id var-sym]
  (conv/db-list-var-states db-info conversation-id (str var-sym)))

(defn diffable-value?
  "True when `v` is a collection type suitable for structural diff."
  [v]
  (or (map? v) (vector? v) (set? v) (sequential? v)))
