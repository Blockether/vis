(ns com.blockether.vis.internal.improve.core-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.improve :as contract]
            [com.blockether.vis.internal.improve.core :as improve]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [next.jdbc :as jdbc]))

(h/use-mem-store! {"improve" true})

(defn- error-type [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defn- project!
  [db]
  (let [id (str (random-uuid))]
    (h/raw-query db
                 {:insert-into :project
                  :values [{:id id :owner_id "local" :name "Project" :created_at 0}]})
    id))

(defn- complaint!
  [db key & [title]]
  (ps/db-council-insert! db
                         {:author_sid "source-session"
                          :activation_id "test"
                          :source "autocomplain"
                          :kind "complain"
                          :title (or title "Failed Python execution")
                          :content "Original diagnostic evidence"
                          :created_at 10
                          :idempotency_key key
                          :fingerprint key
                          :source_ref {:session_id "source-session"
                                       :scope {:turn 2 :iter 3 :next_form 1}}}
                         []
                         false))

(deftest records-and-contract-test
  (let [db
        (h/store)

        record
        (improve/create! db {:title "Investigate" :content "## Analysis\nNot reproduced."})]

    (is (= 1 (get (document/load! "improve") "version")))
    (is (contract/valid? :record record) (pr-str record))
    (is (= "open" (:status record)))
    (is (nil? (:entry_id record)))
    (is (= [record] (improve/list-records db {:project_id nil})))
    (is (= [nil] (improve/project-ids db)))
    (is (nil? (improve/get-record db 999)))
    (doseq [attrs [{:title "  "} {:title "Valid" :source_content "forged"}
                   {:title "Valid" :entry_id 1} {:title "Valid" :project_id "bad"}]]
      (is (= :improve/invalid (error-type #(improve/create! db attrs)))))
    (is (= :improve/invalid (error-type #(improve/list-records db {:limit 201}))))
    (is (= :improve/invalid (error-type #(improve/update! db (:id record) {:source_ref {}}))))))

(deftest record-id-and-review-version-validation-test
  ;; Numeric guards must reject invalid values before coercion or any write.
  (let [db
        (h/store)

        record
        (improve/create! db {:title "Keep this analysis"})]

    (is (= record (improve/get-record db (:id record))))
    (is (nil? (improve/get-record db Long/MAX_VALUE)))
    (doseq [invalid [nil 0 -1 "1" 1.5]]
      (is (= :improve/invalid (error-type #(improve/get-record db invalid))))
      (doseq [expected [{invalid 1} {(:id record) invalid}]]
        (is (= :improve/invalid
               (error-type #(improve/apply-review! db {:expected expected} (constantly true)))))))
    (is (= record (improve/get-record db (:id record))))))

(deftest immutable-intake-test
  (let [db
        (h/store)

        result
        (complaint! db "once")

        record
        (first (improve/list-records db {}))]

    (is (:inserted? result))
    (is (= (:entry_id (:entry result)) (:entry_id record)))
    (is (= "Original diagnostic evidence" (:source_content record)))
    (is (= "source-session" (:session_id record)))
    (is (= {:turn 2 :iter 3 :next_form 1} (get-in record [:source_ref :scope])))
    (is (= "" (:content record)))
    (improve/update! db (:id record) {:title "Human title" :content "Human analysis"})
    (complaint! db "once")
    (is (= 1 (count (improve/list-records db {}))))
    (let [edited (improve/get-record db (:id record))]
      (is (= "Human analysis" (:content edited)))
      (is (= (:source_ref record) (:source_ref edited)))
      (is (= (:source_content record) (:source_content edited))))))

(deftest hierarchy-and-project-test
  (let [db
        (h/store)

        p
        (project! db)

        q
        (project! db)

        parent
        (improve/create! db {:title "Group" :project_id p})

        child
        (improve/create! db {:title "Child" :project_id p :parent_id (:id parent)})

        leaf
        (improve/create! db {:title "Leaf" :project_id p :parent_id (:id child)})]

    (is (= :improve/invalid
           (error-type #(improve/update! db (:id parent) {:parent_id (:id leaf)}))))
    (is (= :improve/invalid (error-type #(improve/update! db (:id child) {:project_id q}))))
    (is (= p (:project_id (improve/get-record db (:id leaf)))))
    (improve/update! db (:id parent) {:status "closed"})
    (is (every? #(= "closed" (:status %)) (improve/list-records db {})))
    (is (= [] (improve/project-ids db)))
    (is (= :improve/invalid
           (error-type #(improve/create! db
                                         {:title "Open" :project_id p :parent_id (:id parent)}))))
    (improve/update! db (:id leaf) {:status "open"})
    (is (every? #(= "open" (:status %)) (improve/list-records db {})))
    (improve/update! db (:id parent) {:project_id q})
    (is (every? #(= q (:project_id %)) (improve/list-records db {})))
    (is (= [] (improve/list-records db {:project_id p})))
    (is (= 2 (count (improve/list-records db {:after (:id parent) :limit 2}))))
    (is (= [q] (improve/project-ids db)))))

(deftest optimistic-edit-test
  (let [db
        (h/store)

        record
        (improve/create! db {:title "Record"})]

    (improve/update! db (:id record) {:content "Human edit" :expected_version 1})
    (is (= :improve/conflict
           (error-type
             #(improve/update! db (:id record) {:content "Lost edit" :expected_version 1}))))
    (is (= "Human edit" (:content (improve/get-record db (:id record)))))
    (is (= 2 (:version (improve/get-record db (:id record)))))
    (is (= :improve/not-found (error-type #(improve/update! db 999 {:title "Missing"}))))))

(deftest automatic-review-test
  (let [db
        (h/store)

        a
        (improve/create! db {:title "A" :content "Human analysis"})

        b
        (improve/create! db {:title "B"})

        proposal
        {:expected {(:id a) 1 (:id b) 1}
         :updates [{:id (:id a) :content "Human analysis\n\n## Automatic review\nNot reproduced."}]
         :groups [{:title "Related"
                   :project_id nil
                   :content "Grouping rationale"
                   :children [(:id a) (:id b)]}]}

        result
        (improve/apply-review! db proposal (constantly true))

        group
        (last (:records result))]

    (is (= 3 (count (:records result))))
    (is (= (:id group) (:parent_id (improve/get-record db (:id a)))))
    (is (= (:id group) (:parent_id (improve/get-record db (:id b)))))
    (is (= "open" (:status group)))
    (is (= :improve/conflict (error-type #(improve/apply-review! db proposal (constantly true)))))))

(deftest automatic-review-rollback-test
  (let [db
        (h/store)

        a
        (improve/create! db {:title "A"})

        b
        (improve/create! db {:title "B"})

        proposal
        {:expected {(:id a) 1 (:id b) 1}
         :updates [{:id (:id a) :content "Proposed"}]
         :groups [{:title "Group" :children [(:id a) (:id b)]}]}

        gate
        (atom 0)]

    (is (= :improve/conflict
           (error-type #(improve/apply-review! db
                                               proposal
                                               (fn []
                                                 (= 1 (swap! gate inc)))))))
    (is (= 2 (count (improve/list-records db {}))))
    (is (= a (improve/get-record db (:id a))))
    (is (= :improve/invalid
           (error-type #(improve/apply-review! db
                                               (assoc proposal
                                                 :updates [{:id (:id a) :status "closed"}])
                                               (constantly true)))))
    (is (= :improve/invalid
           (error-type
             #(improve/apply-review! db (assoc proposal :expected {(:id a) 1}) (constantly true)))))
    (is (= a (improve/get-record db (:id a))))))

(deftest automatic-cross-project-rollback-test
  (let [db
        (h/store)

        p
        (project! db)

        a
        (improve/create! db {:title "A"})

        b
        (improve/create! db {:title "B" :project_id p})]

    (is (= :improve/invalid
           (error-type #(improve/apply-review! db
                                               {:expected {(:id a) 1 (:id b) 1}
                                                :updates [{:id (:id a) :content "Proposed"}]
                                                :groups [{:title "Group"
                                                          :children [(:id a) (:id b)]}]}
                                               (constantly true)))))
    (is (= [a b] (improve/list-records db {})))))

(deftest populated-store-backfill-test
  (let [dir
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-improve"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        db
        (ps/db-create-connection! (.getPath dir))]

    (try (complaint! db "retained")
         ;; Simulate a populated pre-workflow canonical store, then really close/reopen it.
         (jdbc/execute! (:datasource db) ["DROP TABLE improve_record"])
         (ps/db-dispose-connection! db)
         (let [reopened (ps/db-create-connection! (.getPath dir))]
           (try (let [record (first (improve/list-records reopened {}))]
                  (is (= 1 (count (improve/list-records reopened {}))))
                  (is (= "Original diagnostic evidence" (:source_content record)))
                  (improve/update! reopened
                                   (:id record)
                                   {:content "Retained human Markdown" :status "closed"}))
                (finally (ps/db-dispose-connection! reopened))))
         (let [again (ps/db-create-connection! (.getPath dir))]
           (try (is (= 1 (count (improve/list-records again {}))))
                (is (= "Retained human Markdown"
                       (:content (first (improve/list-records again {})))))
                (is (= "closed" (:status (first (improve/list-records again {})))))
                (finally (ps/db-dispose-connection! again))))
         (finally (ps/db-dispose-connection! db)
                  (doseq [^java.io.File f (reverse (file-seq dir))]
                    (.delete f))))))

(deftest project-deletion-preserves-workflow-test
  (let [db
        (h/store)

        p
        (project! db)

        group
        (improve/create! db {:title "Group" :project_id p :content "Keep analysis"})

        child
        (improve/create! db {:title "Child" :project_id p :parent_id (:id group)})]

    (h/raw-query db {:delete-from :project :where [:= :id p]})
    (is (= 2 (count (improve/list-records db {:project_id nil}))))
    (is (= "Keep analysis" (:content (improve/get-record db (:id group)))))
    (is (= (:id group) (:parent_id (improve/get-record db (:id child)))))))

(deftest automatic-review-preserves-human-grouping-test
  (let [db
        (h/store)

        parent
        (improve/create! db {:title "Human group"})

        child
        (improve/create! db {:title "Child" :parent_id (:id parent)})

        proposal
        {:expected {(:id child) 1} :groups [{:title "Model group" :children [(:id child)]}]}]

    (is (= :improve/conflict (error-type #(improve/apply-review! db proposal (constantly true)))))
    (is (= [parent child] (improve/list-records db {})))))

(deftest complaint-title-cannot-break-intake-test
  (let [db (h/store)]
    ;; Council permits nonempty whitespace titles; stricter editable title validation must not
    ;; reject otherwise valid complaints or prevent a populated store from opening.
    (complaint! db "blank-title" "   ")
    (complaint! db "control-title" "\t\n")
    (is (= 2 (count (improve/list-records db {}))))
    (is (every? #(contract/valid? :record %) (improve/list-records db {})))))
