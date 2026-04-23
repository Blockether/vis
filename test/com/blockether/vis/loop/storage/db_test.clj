(ns com.blockether.vis.loop.storage.db-test
  "Storage layer tests — SQLite in-memory backend.

   Soul/state pattern:
     conversation (soul) → conversation_state (state, forkable)
     iteration_var (soul) → iteration_var_state (state, versioned)"
  (:require
   [com.blockether.vis.loop.storage.db :as db]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [noahtheduke.lazytest :refer [defdescribe it expect]]))

(defn- with-mem-store [f]
  (let [store (core/open-store :memory)]
    (try
      (f store)
      (finally
        (core/close-store store)))))

;; =============================================================================
;; Conversation soul + state
;; =============================================================================

(defdescribe conversation-soul-test
  (it "creates conversation soul + initial state (version 1)"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store
                        {:channel :vis :system-prompt "You are helpful."
                         :model "gpt-4o" :title "Test conv"})
              conv    (db/db-get-conversation store conv-id)]
          (expect (= :conversation (:type conv)))
          (expect (= :vis (:channel conv)))
          (expect (= "You are helpful." (:system-prompt conv)))
          (expect (= "gpt-4o" (:model conv)))
          (expect (= "Test conv" (:title conv)))
          (expect (= 1 (:version conv)))))))

  (it "resolves :latest conversation"
    (with-mem-store
      (fn [store]
        (db/store-conversation! store {:channel :vis :system-prompt "first"})
        (Thread/sleep 2)
        (let [id2    (db/store-conversation! store {:channel :vis :system-prompt "second"})
              latest (db/db-resolve-conversation-id store :latest)]
          (expect (= (second id2) (second latest)))))))

  (it "returns nil for missing conversation"
    (with-mem-store
      (fn [store]
        (expect (nil? (db/db-get-conversation store [:id (java.util.UUID/randomUUID)]))))))

  (it "lists conversations by channel"
    (with-mem-store
      (fn [store]
        (db/store-conversation! store {:channel :vis :title "A"})
        (db/store-conversation! store {:channel :telegram :title "B"})
        (db/store-conversation! store {:channel :vis :title "C"})
        (let [vis-convs (db/db-list-conversations store :vis)
              tg-convs  (db/db-list-conversations store :telegram)]
          (expect (= 2 (count vis-convs)))
          (expect (= 1 (count tg-convs)))
          (expect (= "B" (:title (first tg-convs))))))))

  (it "finds conversation by external id"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store
                        {:channel :telegram :external-id "chat-123"})
              found   (db/db-find-conversation-by-external store :telegram "chat-123")]
          (expect (= (second conv-id) (second found)))
          (expect (nil? (db/db-find-conversation-by-external store :telegram "chat-999")))))))

  (it "updates conversation title on latest state"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})]
          (db/db-update-conversation-title! store conv-id "New title")
          (expect (= "New title" (:title (db/db-get-conversation store conv-id)))))))))

;; =============================================================================
;; Fork
;; =============================================================================

(defdescribe conversation-fork-test
  (it "forks a conversation creating a new state"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store
                        {:channel :vis :system-prompt "original" :model "gpt-4o"})
              ;; Add a query to the initial state
              q1-id   (db/store-query! store
                        {:parent-conversation-id conv-id :query "Turn 1" :status :done})
              ;; Fork — new state inherits prompt/model, overrides title
              fork-ref (db/fork-conversation! store conv-id
                         {:fork-after-query-id (second q1-id)
                          :title "Forked branch"})
              ;; After fork, get-conversation returns the latest state (forked)
              conv     (db/db-get-conversation store conv-id)]
          (expect (some? fork-ref))
          (expect (= 2 (:version conv)))
          (expect (= "Forked branch" (:title conv)))
          (expect (= "original" (:system-prompt conv)))))))

  (it "new queries go on the forked state"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis :system-prompt ""})
              q1-id   (db/store-query! store
                        {:parent-conversation-id conv-id :query "Turn 1" :status :done})
              _       (db/fork-conversation! store conv-id
                        {:fork-after-query-id (second q1-id) :title "Fork"})
              _       (db/store-query! store
                        {:parent-conversation-id conv-id :query "Turn 2 (forked)" :status :done})
              ;; Queries on the latest (forked) state — only Turn 2
              queries (db/db-list-conversation-queries store conv-id)]
          (expect (= 1 (count queries)))
          (expect (= "Turn 2 (forked)" (:text (first queries)))))))))

;; =============================================================================
;; Query
;; =============================================================================

(defdescribe query-lifecycle-test
  (it "stores and lists queries"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              _       (db/store-query! store
                        {:parent-conversation-id conv-id :query "What is 2+2?" :status :running})
              _       (db/store-query! store
                        {:parent-conversation-id conv-id :query "What is 3+3?" :status :running})
              queries (db/db-list-conversation-queries store conv-id)]
          (expect (= 2 (count queries)))
          (expect (= "What is 2+2?" (:text (first queries))))))))

  (it "updates a query with final result"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})]
          (db/update-query! store q-id
            {:answer "42" :iterations 3 :duration-ms 1500 :status :done
             :tokens {:input 100 :output 50 :reasoning 20 :cached 10}
             :cost {:total-cost 0.005 :model "gpt-4o"}})
          (let [q (first (db/db-list-conversation-queries store conv-id))]
            (expect (= :done (:status q)))
            (expect (= 3 (:iterations q)))
            (expect (= 100 (:input-tokens q)))))))))

;; =============================================================================
;; Iteration + Execution
;; =============================================================================

(defdescribe iteration-execution-test
  (it "stores iteration with executions as separate rows"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})
              iter-id (db/store-iteration! store
                        {:query-id   q-id
                         :executions [{:code "(+ 1 1)" :result 2 :execution-time-ms 5}
                                      {:code "(* 3 4)" :result 12 :execution-time-ms 3}]
                         :thinking   "Let me compute"
                         :duration-ms 50})
              iters   (db/db-list-query-iterations store q-id)
              execs   (db/db-list-executions store iter-id)]
          (expect (= 1 (count iters)))
          (expect (= "Let me compute" (:thinking (first iters))))
          (expect (= 2 (count execs)))
          (expect (= "(+ 1 1)" (:code (first execs))))
          (expect (= 2 (:result (first execs))))
          (expect (= 0 (:position (first execs))))
          (expect (= 1 (:position (second execs))))))))

  (it "stores execution errors and metadata"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})
              iter-id (db/store-iteration! store
                        {:query-id   q-id
                         :executions [{:code "(/ 1 0)" :error "Divide by zero"
                                       :timeout? true :stdout "debug" :stderr "warn"}]
                         :duration-ms 10})
              e       (first (db/db-list-executions store iter-id))]
          (expect (= "Divide by zero" (:error e)))
          (expect (true? (:timeout? e)))
          (expect (= "debug" (:stdout e))))))))

;; =============================================================================
;; Iteration var soul + state
;; =============================================================================

(defdescribe iteration-var-soul-test
  (it "creates var soul and first state"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})
              iter-id (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 42 :code "(def x 42)"}]})
              vars    (db/db-list-iteration-vars store iter-id)]
          (expect (= 1 (count vars)))
          (expect (= "x" (:name (first vars))))
          (expect (= 42 (:value (first vars))))
          (expect (= 1 (:version (first vars))))))))

  (it "reuses soul across iterations, increments version"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 1 :code "(def x 1)"}]})
              iter2   (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 99 :code "(def x 99)"}]})
              vars    (db/db-list-iteration-vars store iter2)]
          (expect (= 99 (:value (first vars))))
          (expect (= 2 (:version (first vars))))))))

  (it "soul persists across queries"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q1-id   (db/store-query! store
                        {:parent-conversation-id conv-id :query "turn 1" :status :done})
              _       (db/store-iteration! store
                        {:query-id q1-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 1 :code "(def x 1)"}]})
              q2-id   (db/store-query! store
                        {:parent-conversation-id conv-id :query "turn 2" :status :done})
              iter2   (db/store-iteration! store
                        {:query-id q2-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 100 :code "(def x 100)"}]})
              vars    (db/db-list-iteration-vars store iter2)]
          (expect (= 100 (:value (first vars))))
          (expect (= 2 (:version (first vars))))))))

  (it "latest var registry — max version per soul"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :done})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 1} {:name "y" :value "hello"}]})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 99}]})
              reg     (db/db-latest-var-registry store conv-id)]
          (expect (= 99 (:value (get reg 'x))))
          (expect (= 2 (:version (get reg 'x))))
          (expect (= "hello" (:value (get reg 'y))))
          (expect (= 1 (:version (get reg 'y))))))))

  (it "full version history for a var"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :done})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 1}]})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 50}]})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [] :duration-ms 0
                         :vars [{:name "x" :value 99}]})
              history (db/db-var-history store conv-id 'x)]
          (expect (= 3 (count history)))
          (expect (= [1 50 99] (mapv :value history)))
          (expect (= [1 2 3] (mapv :version history))))))))

;; =============================================================================
;; Cascade delete
;; =============================================================================

(defdescribe cascade-delete-test
  (it "deletes conversation soul and everything underneath"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "test" :status :running})
              _       (db/store-iteration! store
                        {:query-id q-id :executions [{:code "1" :result 1}] :duration-ms 0
                         :vars [{:name "x" :value 1}]})]
          (db/delete-conversation-tree! store (second conv-id))
          (expect (nil? (db/db-get-conversation store conv-id)))
          (expect (= [] (db/db-list-conversation-queries store conv-id)))
          (expect (= {} (db/db-latest-var-registry store conv-id))))))))

;; =============================================================================
;; Derived views
;; =============================================================================

(defdescribe derived-views-test
  (it "builds query history"
    (with-mem-store
      (fn [store]
        (let [conv-id (db/store-conversation! store {:channel :vis})
              q-id    (db/store-query! store
                        {:parent-conversation-id conv-id :query "What is Clojure?" :status :done})
              _       (db/store-iteration! store
                        {:query-id q-id
                         :executions [{:code "(def lang \"Clojure\")" :result "Clojure"}]
                         :answer "Clojure is a Lisp"
                         :duration-ms 100})
              history (db/db-query-history store conv-id)]
          (expect (= 1 (count history)))
          (expect (= "What is Clojure?" (:query (first history))))
          (expect (= 1 (:iterations (first history)))))))))
