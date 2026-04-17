(ns com.blockether.vis.loop.sci.conversation
  "Tools that browse prior query state in the current conversation:
   conversation-history / conversation-code / conversation-results. All
   read through the persistence layer via a conversation-ref; nil db-info
   degrades to empty vectors."
  (:require
   [com.blockether.vis.loop.storage.db :as db]))

(defn make-conversation-history-fn
  "Creates conversation-history for browsing prior query summaries in a conversation."
  [db-info conversation-ref]
  (fn conversation-history
    ([]
     (conversation-history nil))
    ([n]
     (if db-info
       (let [history (db/db-query-history db-info conversation-ref)
             selected (if (some? n) (take-last (max 0 (long n)) history) history)]
         (mapv #(select-keys % [:query-pos :query-id :text :answer-preview :status :iterations :key-vars :created-at])
           selected))
       []))))

(defn- resolve-query-ref
  [db-info conversation-ref query-selector]
  (let [history (db/db-query-history db-info conversation-ref)]
    (cond
      (nil? query-selector) (some-> history last :query-ref)
      (integer? query-selector) (some->> history (filter #(= (:query-pos %) query-selector)) first :query-ref)
      (and (vector? query-selector) (= :id (first query-selector))) [:id (second query-selector)]
      (uuid? query-selector) [:id query-selector]
      :else nil)))

(defn make-conversation-code-fn
  "Creates conversation-code for browsing prior query code blocks."
  [db-info conversation-ref]
  (fn conversation-code
    ([query-selector]
     (if db-info
       (if-let [query-ref (resolve-query-ref db-info conversation-ref query-selector)]
         (db/db-query-code db-info query-ref)
         [])
       []))))

(defn make-conversation-results-fn
  "Creates conversation-results for browsing prior query results and restorable vars."
  [db-info conversation-ref]
  (fn conversation-results
    ([query-selector]
     (if db-info
       (if-let [query-ref (resolve-query-ref db-info conversation-ref query-selector)]
         (db/db-query-results db-info query-ref)
         [])
       []))))
