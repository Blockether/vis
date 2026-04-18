(ns com.blockether.vis.loop.knowledge.ontology-test
  (:require
   [com.blockether.vis.test-helpers :as h]
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
