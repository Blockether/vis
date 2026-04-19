(ns com.blockether.vis.loop.conversations.persistence-sweep-test
  "Regression: on crash/restart, `query_attrs.status='running'` rows are
   zombies — the process that owned them is gone, and without a sweep they
   show as stuck spinners in the sidebar forever. `sweep-orphaned-running-queries!`
   is called on web boot BEFORE any new turn is accepted, so every such row
   is unambiguously an orphan and can be rewritten to `:interrupted`."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.conversations.persistence :as conv-persistence]
    [com.blockether.vis.loop.storage.db :as rlm-db]))

(defn- make-temp-db []
  (rlm-db/create-rlm-conn :temp))

(defn- make-conv! [db]
  (rlm-db/store-conversation! db {:system-prompt "test" :model "test"}))

(defn- make-query! [db conv-ref {:keys [status text]}]
  (rlm-db/store-query! db
    {:conversation-ref conv-ref
     :text             (or text "hello")
     :messages         [{:role "user" :content (or text "hello")}]
     :status           status
     :iterations       0
     :duration-ms      0}))

(defdescribe sweep-orphaned-running-queries-test
  (describe "sweep-orphaned-running-queries!"
    (it "rewrites :running rows to :interrupted and stamps an answer"
      (let [db (make-temp-db)
            conv-ref (make-conv! db)
            running-ref (make-query! db conv-ref {:status :running :text "zombie"})
            success-ref (make-query! db conv-ref {:status :success :text "ok"})
            swept (conv-persistence/sweep-orphaned-running-queries! db)
            remaining-running (rlm-db/db-list-queries db {:status :running})
            swept-row (first (rlm-db/db-list-queries db {:status :interrupted}))
            still-success (first (rlm-db/db-list-queries db {:status :success}))]
        (expect (= 1 swept))
        (expect (empty? remaining-running))
        (expect (= (second running-ref) (:id swept-row)))
        (expect (seq (:answer swept-row)))
        (expect (= (second success-ref) (:id still-success)))
        (rlm-db/dispose-rlm-conn! db)))
    (it "is a no-op when no :running rows exist"
      (let [db (make-temp-db)
            conv-ref (make-conv! db)
            _ (make-query! db conv-ref {:status :success :text "done"})
            swept (conv-persistence/sweep-orphaned-running-queries! db)]
        (expect (zero? swept))
        (rlm-db/dispose-rlm-conn! db)))))
