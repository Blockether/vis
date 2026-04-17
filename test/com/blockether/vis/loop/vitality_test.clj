(ns com.blockether.vis.loop.vitality-test
  (:require
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- temp-db []
  (#'rlm-db/create-rlm-conn :temp))

(defn- dispose-db! [db-info]
  (#'rlm-db/dispose-rlm-conn! db-info))

(defn- seed-doc!
  [db-info]
  (let [{:keys [document-id]}
        (rlm-db/db-store-pageindex-document! db-info
          {:name "vitality-test-doc"
           :extension "md"
           :pages [{:index 0 :nodes [{:id "p0n0" :type :paragraph :content "Page 0"}]}
                   {:index 1 :nodes [{:id "p1n0" :type :paragraph :content "Page 1"}]}]
           :toc []})
        page-ids (->> (rlm-db/db-document-page-nodes-full db-info document-id)
                   (map :page-id)
                   distinct
                   sort
                   vec)]
    {:document-id document-id
     :page-ids page-ids}))

(defdescribe vitality-integration-test
  (describe "record-page-access!"
    (it "records access on the source page"
      (let [db-info (temp-db)]
        (try
          (let [{:keys [page-ids]} (seed-doc! db-info)
                [source-page] page-ids
                before (rlm-db/get-page-vitality db-info source-page)]
            (rlm-db/record-page-access! db-info source-page 0.25)
            (let [after (rlm-db/get-page-vitality db-info source-page)]
              (expect (> (:access-count after)
                        (:access-count before)))))
          (finally (dispose-db! db-info))))))

  (describe "Q-value updates"
    (it "moves page Q-value toward observed rewards"
      (let [db-info (temp-db)]
        (try
          (let [{:keys [page-ids]} (seed-doc! db-info)
                page-id (first page-ids)
                q0 (rlm-db/get-page-q-value db-info page-id)]
            (rlm-db/finalize-q-updates! db-info [page-id] 1.0)
            (let [q1 (rlm-db/get-page-q-value db-info page-id)]
              (expect (> q1 q0))
              (rlm-db/finalize-q-updates! db-info [page-id] 0.0)
              (let [q2 (rlm-db/get-page-q-value db-info page-id)]
                (expect (< q2 q1)))))
          (finally (dispose-db! db-info)))))))
