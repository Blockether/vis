(ns com.blockether.vis.internal.improve.core-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.improve :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.improve.core :as improve]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]
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

(defdescribe
  records-and-contract-test
  (it "records and contract"
      (let [db
            (h/store)

            record
            (improve/create! db {:title "Investigate" :content "## Analysis\nNot reproduced."})]

        (expect (document/valid? "improve" (wire/->wire record)))
        (expect (contract/valid? :record record) (pr-str record))
        (expect (= "open" (:status record)))
        (expect (nil? (:entry_id record)))
        (expect (= [record] (improve/list-records db {:project_id nil})))
        (expect (= [nil] (improve/project-ids db)))
        (expect (nil? (improve/get-record db 999)))
        (doseq [attrs [{:title "  "} {:title "Valid" :source_content "forged"}
                       {:title "Valid" :entry_id 1} {:title "Valid" :project_id "bad"}]]
          (expect (= :improve/invalid (error-type #(improve/create! db attrs)))))
        (expect (= :improve/invalid (error-type #(improve/list-records db {:limit 201}))))
        (expect (= :improve/invalid
                   (error-type #(improve/update! db (:id record) {:source_ref {}})))))))

(defdescribe record-id-and-review-version-validation-test
             (it "record id and review version validation"
                 ;; Numeric guards must reject invalid values before coercion or any write.
                 (let [db
                       (h/store)

                       record
                       (improve/create! db {:title "Keep this analysis"})]

                   (expect (= record (improve/get-record db (:id record))))
                   (expect (nil? (improve/get-record db Long/MAX_VALUE)))
                   (doseq [invalid [nil 0 -1 "1" 1.5]]
                     (expect (= :improve/invalid (error-type #(improve/get-record db invalid))))
                     (doseq [expected [{invalid 1} {(:id record) invalid}]]
                       (expect (= :improve/invalid
                                  (error-type #(improve/apply-review! db
                                                                      {:expected expected}
                                                                      (constantly true)))))))
                   (expect (= record (improve/get-record db (:id record)))))))

(defdescribe immutable-intake-test
             (it
               "immutable intake"
               (let [db
                     (h/store)

                     result
                     (complaint! db "once")

                     record
                     (first (improve/list-records db {}))]

                 (expect (:inserted? result))
                 (expect (= (:entry_id (:entry result)) (:entry_id record)))
                 (expect (= "Original diagnostic evidence" (:source_content record)))
                 (expect (= "source-session" (:session_id record)))
                 (expect (= {:turn 2 :iter 3 :next_form 1} (get-in record [:source_ref :scope])))
                 (expect (= "" (:content record)))
                 (improve/update! db (:id record) {:title "Human title" :content "Human analysis"})
                 (complaint! db "once")
                 (expect (= 1 (count (improve/list-records db {}))))
                 (let [edited (improve/get-record db (:id record))]
                   (expect (= "Human analysis" (:content edited)))
                   (expect (= (:source_ref record) (:source_ref edited)))
                   (expect (= (:source_content record) (:source_content edited)))))))

(defdescribe
  hierarchy-and-project-test
  (it
    "hierarchy and project"
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

      (expect (= :improve/invalid
                 (error-type #(improve/update! db (:id parent) {:parent_id (:id leaf)}))))
      (expect (= :improve/invalid (error-type #(improve/update! db (:id child) {:project_id q}))))
      (expect (= p (:project_id (improve/get-record db (:id leaf)))))
      (improve/update! db (:id parent) {:status "closed"})
      (expect (every? #(= "closed" (:status %)) (improve/list-records db {})))
      (expect (= [] (improve/project-ids db)))
      (expect (= :improve/invalid
                 (error-type
                   #(improve/create! db {:title "Open" :project_id p :parent_id (:id parent)}))))
      (improve/update! db (:id leaf) {:status "open"})
      (expect (every? #(= "open" (:status %)) (improve/list-records db {})))
      (improve/update! db (:id parent) {:project_id q})
      (expect (every? #(= q (:project_id %)) (improve/list-records db {})))
      (expect (= [] (improve/list-records db {:project_id p})))
      (expect (= 2 (count (improve/list-records db {:after (:id parent) :limit 2}))))
      (expect (= [q] (improve/project-ids db))))))

(defdescribe
  optimistic-edit-test
  (it "optimistic edit"
      (let [db
            (h/store)

            record
            (improve/create! db {:title "Record"})]

        (improve/update! db (:id record) {:content "Human edit" :expected_version 1})
        (expect (= :improve/conflict
                   (error-type #(improve/update! db
                                                 (:id record)
                                                 {:content "Lost edit" :expected_version 1}))))
        (expect (= "Human edit" (:content (improve/get-record db (:id record)))))
        (expect (= 2 (:version (improve/get-record db (:id record)))))
        (expect (= :improve/not-found (error-type #(improve/update! db 999 {:title "Missing"})))))))

(defdescribe
  automatic-review-test
  (it "automatic review"
      (let [db
            (h/store)

            a
            (improve/create! db {:title "A" :content "Human analysis"})

            b
            (improve/create! db {:title "B"})

            proposal
            {:expected {(:id a) 1 (:id b) 1}
             :updates [{:id (:id a)
                        :content "Human analysis\n\n## Automatic review\nNot reproduced."}]
             :groups [{:title "Related"
                       :project_id nil
                       :content "Grouping rationale"
                       :children [(:id a) (:id b)]}]}

            result
            (improve/apply-review! db proposal (constantly true))

            group
            (last (:records result))]

        (expect (= 3 (count (:records result))))
        (expect (= (:id group) (:parent_id (improve/get-record db (:id a)))))
        (expect (= (:id group) (:parent_id (improve/get-record db (:id b)))))
        (expect (= "open" (:status group)))
        (expect (= :improve/conflict
                   (error-type #(improve/apply-review! db proposal (constantly true))))))))

(defdescribe
  automatic-review-rollback-test
  (it
    "automatic review rollback"
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

      (expect (= :improve/conflict
                 (error-type #(improve/apply-review! db
                                                     proposal
                                                     (fn []
                                                       (= 1 (swap! gate inc)))))))
      (expect (= 2 (count (improve/list-records db {}))))
      (expect (= a (improve/get-record db (:id a))))
      (expect (= :improve/invalid
                 (error-type #(improve/apply-review! db
                                                     (assoc proposal
                                                       :updates [{:id (:id a) :status "closed"}])
                                                     (constantly true)))))
      (expect (= :improve/invalid
                 (error-type #(improve/apply-review! db
                                                     (assoc proposal :expected {(:id a) 1})
                                                     (constantly true)))))
      (expect (= a (improve/get-record db (:id a)))))))

(defdescribe automatic-cross-project-rollback-test
             (it "automatic cross project rollback"
                 (let [db
                       (h/store)

                       p
                       (project! db)

                       a
                       (improve/create! db {:title "A"})

                       b
                       (improve/create! db {:title "B" :project_id p})]

                   (expect (= :improve/invalid
                              (error-type #(improve/apply-review!
                                             db
                                             {:expected {(:id a) 1 (:id b) 1}
                                              :updates [{:id (:id a) :content "Proposed"}]
                                              :groups [{:title "Group"
                                                        :children [(:id a) (:id b)]}]}
                                             (constantly true)))))
                   (expect (= [a b] (improve/list-records db {}))))))

(defdescribe
  populated-store-backfill-test
  (it "populated store backfill"
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
                      (expect (= 1 (count (improve/list-records reopened {}))))
                      (expect (= "Original diagnostic evidence" (:source_content record)))
                      (improve/update! reopened
                                       (:id record)
                                       {:content "Retained human Markdown" :status "closed"}))
                    (finally (ps/db-dispose-connection! reopened))))
             (let [again (ps/db-create-connection! (.getPath dir))]
               (try (expect (= 1 (count (improve/list-records again {}))))
                    (expect (= "Retained human Markdown"
                               (:content (first (improve/list-records again {})))))
                    (expect (= "closed" (:status (first (improve/list-records again {})))))
                    (finally (ps/db-dispose-connection! again))))
             (finally (ps/db-dispose-connection! db)
                      (doseq [^java.io.File f (reverse (file-seq dir))]
                        (.delete f)))))))

(defdescribe project-deletion-preserves-workflow-test
             (it "project deletion preserves workflow"
                 (let [db
                       (h/store)

                       p
                       (project! db)

                       group
                       (improve/create! db {:title "Group" :project_id p :content "Keep analysis"})

                       child
                       (improve/create! db {:title "Child" :project_id p :parent_id (:id group)})]

                   (h/raw-query db {:delete-from :project :where [:= :id p]})
                   (expect (= 2 (count (improve/list-records db {:project_id nil}))))
                   (expect (= "Keep analysis" (:content (improve/get-record db (:id group)))))
                   (expect (= (:id group) (:parent_id (improve/get-record db (:id child))))))))

(defdescribe automatic-review-preserves-human-grouping-test
             (it "automatic review preserves human grouping"
                 (let [db
                       (h/store)

                       parent
                       (improve/create! db {:title "Human group"})

                       child
                       (improve/create! db {:title "Child" :parent_id (:id parent)})

                       proposal
                       {:expected {(:id child) 1}
                        :groups [{:title "Model group" :children [(:id child)]}]}]

                   (expect (= :improve/conflict
                              (error-type #(improve/apply-review! db proposal (constantly true)))))
                   (expect (= [parent child] (improve/list-records db {}))))))

(defdescribe complaint-title-cannot-break-intake-test
             (it "complaint title cannot break intake"
                 (let [db (h/store)]
                   ;; Council permits nonempty whitespace titles; stricter editable title validation must not
                   ;; reject otherwise valid complaints or prevent a populated store from opening.
                   (complaint! db "blank-title" "   ")
                   (complaint! db "control-title" "\t\n")
                   (expect (= 2 (count (improve/list-records db {}))))
                   (expect (every? #(contract/valid? :record %) (improve/list-records db {}))))))
