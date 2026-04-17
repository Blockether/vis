(ns com.blockether.vis.loop.memory-system-test
  (:require
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [com.blockether.vis.loop.sci.shared :as tools-shared]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- temp-db []
  (#'rlm-db/create-rlm-conn :temp))

(defn- dispose-db! [db-info]
  (#'rlm-db/dispose-rlm-conn! db-info))

(defn- seed-doc!
  [db-info]
  (let [{:keys [document-id]}
        (rlm-db/db-store-pageindex-document! db-info
          {:name "memory-test-doc"
           :extension "md"
           :pages [{:index 0 :nodes [{:id "m0" :type :paragraph :content "Zero"}]}
                   {:index 1 :nodes [{:id "m1" :type :paragraph :content "One"}]}
                   {:index 2 :nodes [{:id "m2" :type :paragraph :content "Two"}]}]
           :toc []})
        page-ids (->> (rlm-db/db-document-page-nodes-full db-info document-id)
                   (map :page-id)
                   distinct
                   sort
                   vec)]
    {:document-id document-id
     :page-ids page-ids}))

(defn- seed-ranked-doc!
  [db-info]
  (let [{:keys [document-id]}
        (rlm-db/db-store-pageindex-document! db-info
          {:name "memory-ranked-doc"
           :extension "md"
           :pages [{:index 0 :nodes [{:id "r0" :type :paragraph :content "alpha common topic"}]}
                   {:index 1 :nodes [{:id "r1" :type :paragraph :content "beta common topic"}]}]
           :toc []})
        nodes (rlm-db/db-document-page-nodes-full db-info document-id)
        alpha-page-id (:page-id (first (filter #(re-find #"alpha" (or (:content %) "")) nodes)))
        beta-page-id (:page-id (first (filter #(re-find #"beta" (or (:content %) "")) nodes)))]
    {:document-id document-id
     :alpha-page-id alpha-page-id
     :beta-page-id beta-page-id}))

(defdescribe memory-system-integration-test
  (describe "cooccurrence"
    (it "records and returns positive cooccurrence boosts"
      (let [db-info (temp-db)]
        (try
          (let [{:keys [page-ids]} (seed-doc! db-info)
                [p0 p1 p2] page-ids]
            (rlm-db/record-cooccurrence! db-info p0 p1)
            (rlm-db/record-cooccurrence! db-info p0 p1)
            (let [single-boost (rlm-db/get-cooccurrence-boost db-info p1 #{p0})
                  batch-boosts (rlm-db/batch-cooccurrence-boosts db-info [p1 p2] #{p0})
                  recent-pages (rlm-db/recently-accessed-page-ids db-info)]
              (expect (> single-boost 0.0))
              (expect (> (get batch-boosts p1 0.0) 0.0))
              (expect (number? (get batch-boosts p2 0.0)))
              (expect (contains? recent-pages p0))))
          (finally (dispose-db! db-info))))))

  (describe "document certainty"
    (it "increases on access and decreases after reindex jump"
      (let [db-info (temp-db)]
        (try
          (let [{:keys [document-id]} (seed-doc! db-info)
                c0 (:certainty (rlm-db/document-certainty db-info document-id))]
            (rlm-db/record-document-access! db-info document-id 2.0)
            (let [c1 (:certainty (rlm-db/document-certainty db-info document-id))]
              (expect (> c1 c0))
              (rlm-db/reindex-certainty-jump! db-info document-id 8.0)
              (let [c2 (:certainty (rlm-db/document-certainty db-info document-id))]
                (expect (< c2 c1))
                (rlm-db/decay-document-certainty! db-info document-id)
                (let [c3 (:certainty (rlm-db/document-certainty db-info document-id))]
                  (expect (number? c3))
                  (expect (<= c3 c1))))))
          (finally (dispose-db! db-info))))))

  (describe "search-documents end-to-end"
    (it "targeted searches shift later mixed-query ranking"
      (let [db-info (temp-db)]
        (try
          (let [{:keys [document-id alpha-page-id beta-page-id]} (seed-ranked-doc! db-info)
                search-documents (tools-shared/make-search-documents-fn db-info)]
            (dotimes [_ 8]
              (search-documents "alpha" {:in :pages :top-k 5 :document-id document-id}))
            (let [ranked (rlm-db/db-search-page-nodes db-info "common"
                           {:top-k 2 :document-id document-id})
                  top-page-id (:page-id (first ranked))
                  second-page-id (:page-id (second ranked))]
              (expect (= alpha-page-id top-page-id))
              (expect (= beta-page-id second-page-id))))
          (finally (dispose-db! db-info)))))))
