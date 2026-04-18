(ns com.blockether.vis.loop.knowledge.ontology-test
  (:require
   [com.blockether.vis.test-helpers :as h]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.loop.storage.db :as db]
   [com.blockether.vis.loop.knowledge.ontology :as ontology]
   [com.blockether.vis.loop.storage.sqlite.concept-graph :as cg]
   [com.blockether.vis.loop.storage.sqlite.core :as sq-core]
   [lazytest.core :refer [defdescribe describe it expect]]))

(defdescribe concept-removal-rationale-test
  (describe "remove-concept requires rationale"
    (it "throws when removing without rationale"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Schema" :definition "A broad pattern" :group-name "Core"})]
            (expect (try
                      (db/set-concept-status! db-info cid "removed")
                      false
                      (catch Exception e
                        (= :rlm/removal-rationale-required
                          (:type (ex-data e))))))))))

    (it "throws when removing with blank rationale"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Schema Mode" :definition "An emotional state" :group-name "Core"})]
            (expect (try
                      (db/set-concept-status! db-info cid "removed" "  ")
                      false
                      (catch Exception e
                        (= :rlm/removal-rationale-required
                          (:type (ex-data e))))))))))

    (it "succeeds when removing with rationale"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Coping Style" :definition "A behavioral response" :group-name "Core"})]
            (db/set-concept-status! db-info cid "removed"
              "Too generic — overlaps with Schema Mode and not domain-specific enough")
            (let [active (db/list-concepts db-info)]
              (expect (empty? (filter #(= (:term %) "Coping Style") active))))))))

    (it "stores rationale in the DB"
      (h/with-temp-env
        (fn [env]
          (let [db-info  (:db-info env)
                cid      (cg/store-concept! db-info
                           {:term "Surrender" :definition "Giving in to schema" :group-name "Coping"})
                reason   "Redundant with Schema Surrender mode — use that instead"]
            (db/set-concept-status! db-info cid "removed" reason)
            (let [row (first (sq-core/query! db-info
                               ["SELECT status, removal_rationale FROM concept WHERE id = ?" cid]))]
              (expect (= "removed" (:status row)))
              (expect (= reason (:removal_rationale row))))))))

    (it "clears rationale when reactivating"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Overcompensation" :definition "Fighting the schema" :group-name "Coping"})]
            (db/set-concept-status! db-info cid "removed" "Testing removal")
            (db/set-concept-status! db-info cid "active")
            (let [row (first (sq-core/query! db-info
                               ["SELECT status, removal_rationale FROM concept WHERE id = ?" cid]))]
              (expect (= "active" (:status row)))
              (expect (nil? (:removal_rationale row)))))))))

  (describe "sandbox tools enforce rationale"
    (it "remove-concept tool throws without rationale"
      (h/with-temp-env
        (fn [env]
          (let [db-info   (:db-info env)
                _cid      (cg/store-concept! db-info
                            {:term "Avoidance" :definition "Avoiding triggers" :group-name "Coping"})
                bindings  (ontology/make-concept-graph-bindings db-info)
                remove-fn (get bindings 'remove-concept)]
            ;; nil rationale — must throw
            (expect (try
                      (remove-fn "Avoidance" nil)
                      false
                      (catch Exception e
                        (= :rlm/removal-rationale-required
                          (:type (ex-data e))))))
            ;; with rationale — must succeed
            (let [result (remove-fn "Avoidance" "Subsumed by Schema Avoidance mode")]
              (expect (= "Avoidance" (:removed result))))))))))

;; =============================================================================
;; Document ingestion + page concept extraction
;; =============================================================================

(defn- make-test-legal-document []
  {:name "test-contract"
   :extension "pdf"
   :title "Master Services Agreement"
   :abstract "Agreement between Acme Corp and Widget Inc for software services."
   :pages [{:index 0
            :nodes [{:type :heading :id "h1" :level "h1"
                     :content "1. DEFINITIONS AND PARTIES"}
                    {:type :paragraph :id "p1"
                     :content "This Master Services Agreement is entered into between Acme Corp (Provider) and Widget Inc (Client). Services means the software development services described in Exhibit A. Confidential Information means any non-public information disclosed by either party."}]}
           {:index 1
            :nodes [{:type :heading :id "h2" :level "h1"
                     :content "2. OBLIGATIONS AND PAYMENT"}
                    {:type :paragraph :id "p2"
                     :content "Provider shall deliver Services as defined in Section 1. Client shall pay Provider within Net 30 days of invoice. Late payments accrue interest at 1.5% per month."}]}
           {:index 2
            :nodes [{:type :heading :id "h3" :level "h1"
                     :content "3. LIMITATION OF LIABILITY"}
                    {:type :paragraph :id "p3"
                     :content "Neither party's aggregate liability under this Agreement shall exceed the total fees paid during the 12 months preceding the claim."}]}]
   :toc [{:type :toc-entry :id "toc-1" :title "Definitions" :target-page 0 :level "l1"}
         {:type :toc-entry :id "toc-2" :title "Obligations" :target-page 1 :level "l1"}
         {:type :toc-entry :id "toc-3" :title "Liability" :target-page 2 :level "l1"}]})

(defdescribe page-concept-extraction-test
  (describe "content SHA change detection"
    (it "computes content SHA deterministically"
      (let [sha1 (ontology/content-sha "hello world")
            sha2 (ontology/content-sha "hello world")
            sha3 (ontology/content-sha "different")]
        (expect (= sha1 sha2))
        (expect (not= sha1 sha3))
        (expect (clojure.string/starts-with? sha1 "sha256:"))))

    (it "returns nil for blank content"
      (expect (nil? (ontology/content-sha nil)))
      (expect (nil? (ontology/content-sha ""))))

    (it "stores and retrieves page content SHA"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                _       (vis/ingest-to-env! env [(make-test-legal-document)])
                pages   (sq-core/query! db-info
                          ["SELECT id, idx FROM page ORDER BY idx"])]
            ;; Initially no SHA
            (expect (nil? (db/get-page-content-sha db-info (:id (first pages)))))
            ;; Set SHA
            (db/update-page-content-sha! db-info (:id (first pages)) "sha256:abc123")
            (expect (= "sha256:abc123" (db/get-page-content-sha db-info (:id (first pages))))))))))

  (describe "extract-page-concepts! skips unchanged pages"
    (it "returns :unchanged when content SHA matches"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                _       (vis/ingest-to-env! env [(make-test-legal-document)])
                pages   (sq-core/query! db-info
                          ["SELECT p.id, p.document_id, p.idx FROM page p ORDER BY p.idx"])
                page    (first pages)
                nodes   (sq-core/query! db-info
                          ["SELECT id, content FROM page_node WHERE page_id = ?" (:id page)])
                content (clojure.string/join "\n" (keep :content nodes))
                node-id (:id (first nodes))]
            ;; Simulate prior extraction by setting SHA
            (let [sha (ontology/content-sha content)]
              (db/update-page-content-sha! db-info (:id page) sha)
              ;; Should skip before LLM call — stub router is fine
              (let [result (ontology/extract-page-concepts!
                             (h/make-stub-router) db-info (:document_id page) (:id page) node-id content)]
                (expect (= :unchanged result)))))))))

  (describe "user edits survive rebuild"
    (it "preserves user_edited concepts"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Custom Term" :definition "User-defined" :group-name "Custom"})]
            (db/set-concept-status! db-info cid "user_edited")
            (expect (= 1 (count (db/list-concepts db-info))))
            (expect (= "user_edited" (:status (first (db/list-concepts db-info))))))))))

  (describe "removed concepts with rationale"
    (it "stores rationale and excludes from active list"
      (h/with-temp-env
        (fn [env]
          (let [db-info (:db-info env)
                cid     (cg/store-concept! db-info
                          {:term "Provider" :definition "The service provider" :group-name "Parties"})
                reason  "Too generic — every contract has a Provider. Not a domain concept."]
            (db/set-concept-status! db-info cid "removed" reason)
            ;; Not in active list
            (expect (empty? (db/list-concepts db-info)))
            ;; But rationale is in DB
            (let [row (first (sq-core/query! db-info
                               ["SELECT term, status, removal_rationale FROM concept WHERE id = ?" cid]))]
              (expect (= "removed" (:status row)))
              (expect (= reason (:removal_rationale row))))))))))
