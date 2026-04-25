(ns com.blockether.vis.persistance.db-test
  "Storage layer tests — V1 schema, in-memory SQLite.

   Each test gets a fresh isolated DB via lazytest's around-each context."
  (:require
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.vis.test-helpers :as h :refer [store raw-count raw-query thaw-blob]]
   [lazytest.core :refer [defdescribe it expect throws?]]
   [sci.core :as sci]))

(h/use-mem-store!)

;; =============================================================================
;; Conversation
;; =============================================================================

(defdescribe conversation-test
  (it "inserts into conversation_soul + conversation_state"
    (let [s  (store)
          id (db/store-conversation! s {:channel :vis :system-prompt "Hi" :model "gpt-4o" :title "T"})
          conv (db/db-get-conversation s id)]
      (expect (= 1 (raw-count s :conversation_soul)))
      (expect (= 1 (raw-count s :conversation_state)))
      (expect (= :vis (:channel conv)))
      (expect (= "Hi" (:system-prompt conv)))
      (expect (= "gpt-4o" (:model conv)))
      (expect (= "T" (:title conv)))
      (expect (= 0 (:version conv)))))

  (it "resolves :latest"
    (let [s (store)]
      (db/store-conversation! s {:channel :vis})
      (Thread/sleep 2)
      (let [id2    (db/store-conversation! s {:channel :vis})
            latest (db/db-resolve-conversation-id s :latest)]
        (expect (= (second id2) (second latest))))))

  (it "lists by channel via metadata JSON"
    (let [s (store)]
      (db/store-conversation! s {:channel :vis :title "A"})
      (db/store-conversation! s {:channel :telegram :title "B"})
      (db/store-conversation! s {:channel :vis :title "C"})
      (expect (= 2 (count (db/db-list-conversations s :vis))))
      (expect (= 1 (count (db/db-list-conversations s :telegram))))))

  (it "finds by external-id via metadata JSON"
    (let [s  (store)
          id (db/store-conversation! s {:channel :telegram :external-id "chat-42"})]
      (expect (= (second id) (second (db/db-find-conversation-by-external s :telegram "chat-42"))))
      (expect (nil? (db/db-find-conversation-by-external s :telegram "nope")))))

  (it "updates title on conversation_state"
    (let [s  (store)
          id (db/store-conversation! s {:channel :vis :title "Old"})]
      (db/db-update-conversation-title! s id "New")
      (expect (= "New" (:title (db/db-get-conversation s id)))))))

;; =============================================================================
;; Fork
;; =============================================================================

(defdescribe fork-test
  (it "creates a new conversation_state row with parent_state_id"
    (let [s    (store)
          cid  (db/store-conversation! s {:channel :vis :system-prompt "v0" :model "gpt-4o"})
          _    (db/fork-conversation! s cid {:title "Branch A"})
          conv (db/db-get-conversation s cid)]
      (expect (= 2 (raw-count s :conversation_state)))
      (expect (= 1 (:version conv)))
      (expect (= "Branch A" (:title conv)))
      (expect (= "v0" (:system-prompt conv)))
      (let [states (raw-query s {:select [:*] :from :conversation_state :order-by [[:version :asc]]})]
        (expect (nil? (:parent_state_id (first states))))
        (expect (some? (:parent_state_id (second states)))))))

  (it "overrides model and system-prompt"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis :system-prompt "old" :model "gpt-4o"})
          _   (db/fork-conversation! s cid {:system-prompt "new" :model "claude-4"})
          conv (db/db-get-conversation s cid)]
      (expect (= "new" (:system-prompt conv)))
      (expect (= "claude-4" (:model conv)))))

  (it "queries on forked state are isolated"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})]
      (db/store-query! s {:parent-conversation-id cid :query "Turn 1" :status :done})
      (db/fork-conversation! s cid {:title "Fork"})
      (db/store-query! s {:parent-conversation-id cid :query "Turn 2" :status :done})
      (let [queries (db/db-list-conversation-queries s cid)]
        (expect (= 1 (count queries)))
        (expect (= "Turn 2" (:text (first queries)))))))

  (it "double fork increments version"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})]
      (db/fork-conversation! s cid {})
      (db/fork-conversation! s cid {})
      (expect (= 2 (:version (db/db-get-conversation s cid))))
      (expect (= 3 (raw-count s :conversation_state))))))

;; =============================================================================
;; Query
;; =============================================================================

(defdescribe query-test
  (it "inserts into query_soul + query_state"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})]
      (db/store-query! s {:parent-conversation-id cid :query "2+2?" :status :running})
      (expect (= 1 (raw-count s :query_soul)))
      (expect (= 1 (raw-count s :query_state)))
      (let [q (first (db/db-list-conversation-queries s cid))]
        (expect (= "2+2?" (:text q)))
        (expect (= :running (:status q))))))

  (it "normalizes :success to done"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/update-query! s qid {:status :success :answer "42"
                                :tokens {:input 100 :output 50}
                                :cost {:total-cost 0.005 :model "gpt-4o"}})
      (let [q (first (db/db-list-conversation-queries s cid))]
        (expect (= :done (:status q)))
        (expect (= "gpt-4o" (:model q))))))

  (it "normalizes :max-iterations to error"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/update-query! s qid {:status :max-iterations})
      (expect (= :error (:status (first (db/db-list-conversation-queries s cid))))))))

;; =============================================================================
;; Retry
;; =============================================================================

(defdescribe retry-test
  (it "creates query_state version 1 with forked_from ref"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "hard" :status :running})]
      (db/update-query! s qid {:status :error})
      (db/retry-query! s qid {:status :running :model "claude-4"})
      (expect (= 1 (raw-count s :query_soul)))
      (expect (= 2 (raw-count s :query_state)))
      (expect (= :running (:status (first (db/db-list-conversation-queries s cid)))))))

  (it "iterations on retry go to new query_state"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/store-iteration! s {:query-id qid :expressions [{:code "1" :result 1}] :duration-ms 10})
      (db/update-query! s qid {:status :error})
      (db/retry-query! s qid {:status :running :model "better"})
      (db/store-iteration! s {:query-id qid :expressions [{:code "2" :result 2}] :duration-ms 5})
      (expect (= 2 (raw-count s :iteration)))
      (expect (= 1 (count (db/db-list-query-iterations s qid)))))))

;; =============================================================================
;; Iteration + stateless expressions
;; =============================================================================

(defdescribe iteration-expression-test
  (it "inserts into iteration + expression_soul(call) + expression_state"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/store-iteration! s {:query-id qid
                              :expressions [{:code "(+ 1 1)" :result 2 :execution-time-ms 5}
                                            {:code "(* 3 4)" :result 12 :execution-time-ms 3}]
                              :thinking "Computing" :duration-ms 50})
      (expect (= 1 (raw-count s :iteration)))
      (expect (= 2 (raw-count s :expression_soul [:= :kind "call"])))
      (expect (= 2 (raw-count s :expression_state
                     [:in :expression_soul_id
                      {:select [:id] :from :expression_soul :where [:= :kind "call"]}])))
      (let [iter (first (db/db-list-query-iterations s qid))]
        (expect (= "Computing" (:thinking iter)))
        (expect (= 0 (:position iter))))))

  (it "stores results as nippy BLOBs"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/store-iteration! s {:query-id qid
                              :expressions [{:code "{:a [1 2]}" :result {:a [1 2]}}]
                              :duration-ms 5})
      (let [rows (raw-query s {:select [:est.result :est.expr]
                               :from [[:expression_state :est]]
                               :join [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
                               :where [:= :es.kind "call"]})]
        (expect (= {:a [1 2]} (thaw-blob (:result (first rows))))))))

  (it "stores fn result as {:rlm/ref :expr}"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/store-iteration! s {:query-id qid
                              :expressions [{:code "(defn f [x] x)" :result (fn [x] x)}]
                              :duration-ms 5})
      (let [rows (raw-query s {:select [:est.result]
                               :from [[:expression_state :est]]
                               :join [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
                               :where [:= :es.kind "call"]})]
        (expect (= {:rlm/ref :expr} (thaw-blob (:result (first rows))))))))

  (it "stores errors with success=0"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/store-iteration! s {:query-id qid
                              :expressions [{:code "(/ 1 0)" :error "Divide by zero"
                                             :stdout "dbg" :stderr "warn"}]
                              :duration-ms 5})
      (let [row (first (raw-query s {:select [:success :error :stdout :stderr]
                                     :from [[:expression_state :est]]
                                     :join [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
                                     :where [:= :es.kind "call"]}))]
        (expect (= 0 (:success row)))
        (expect (= "Divide by zero" (thaw-blob (:error row))))
        (expect (= "dbg" (:stdout row)))
        (expect (= "warn" (:stderr row)))))))

;; =============================================================================
;; Stateful vars
;; =============================================================================

(defdescribe var-test
  (it "inserts expression_soul(var, stateful) + expression_state"
    (let [s    (store)
          cid  (db/store-conversation! s {:channel :vis})
          qid  (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid  (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                       :vars [{:name "x" :value 42 :code "(def x 42)"}]})
          vars (db/db-list-iteration-vars s iid)]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 1 (count vars)))
      (expect (= "x" (:name (first vars))))
      (expect (= 42 (:value (first vars))))
      (expect (= 0 (:version (first vars))))))

  (it "reuses soul, increments version"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 1}]})
          i2  (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 99}]})]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 99 (:value (first (db/db-list-iteration-vars s i2)))))
      (expect (= 1 (:version (first (db/db-list-iteration-vars s i2)))))))

  (it "soul persists across queries"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          q1  (db/store-query! s {:parent-conversation-id cid :query "t1" :status :done})
          _   (db/store-iteration! s {:query-id q1 :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 1}]})
          q2  (db/store-query! s {:parent-conversation-id cid :query "t2" :status :done})
          i2  (db/store-iteration! s {:query-id q2 :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 100}]})]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 100 (:value (first (db/db-list-iteration-vars s i2)))))))

  (it "fn var stores {:rlm/ref :expr}"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "f" :value (fn [x] x) :code "(defn f [x] x)"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      (expect (= {:rlm/ref :expr} (:value v)))
      (expect (= "(defn f [x] x)" (:code v)))))

  (it "stores complex data via nippy"
    (let [s    (store)
          cid  (db/store-conversation! s {:channel :vis})
          qid  (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          data {:users [{:name "Alice"} {:name "Bob"}] :tags #{:a :b} :n 42}
          iid  (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                       :vars [{:name "db" :value data}]})]
      (expect (= data (:value (first (db/db-list-iteration-vars s iid)))))))

  (it "latest var registry"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 1} {:name "y" :value "hi"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 99}]})
          reg (db/db-latest-var-registry s cid)]
      (expect (= 99 (:value (get reg 'x))))
      (expect (= 1 (:version (get reg 'x))))
      (expect (= "hi" (:value (get reg 'y))))
      (expect (= 0 (:version (get reg 'y))))))

  (it "version history"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0 :vars [{:name "x" :value 1}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0 :vars [{:name "x" :value 50}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0 :vars [{:name "x" :value 99}]})
          h   (db/db-var-history s cid 'x)]
      (expect (= 3 (count h)))
      (expect (= [1 50 99] (mapv :value h)))
      (expect (= [0 1 2] (mapv :version h))))))

;; =============================================================================
;; Cascade delete
;; =============================================================================

(defdescribe cascade-delete-test
  (it "deletes soul + all descendants"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "1" :result 1}]
                                      :duration-ms 0 :vars [{:name "x" :value 1}]})]
      (db/delete-conversation-tree! s (second cid))
      (expect (= 0 (raw-count s :conversation_soul)))
      (expect (= 0 (raw-count s :conversation_state)))
      (expect (= 0 (raw-count s :query_soul)))
      (expect (= 0 (raw-count s :query_state)))
      (expect (= 0 (raw-count s :iteration)))
      (expect (= 0 (raw-count s :expression_soul)))
      (expect (= 0 (raw-count s :expression_state))))))

;; =============================================================================
;; Query history
;; =============================================================================

(defdescribe query-history-test
  (it "builds ordered history with iteration counts"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "What?" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :answer "A Lisp" :duration-ms 100})
          _   (db/store-iteration! s {:query-id qid :expressions [] :answer "JVM Lisp" :duration-ms 50})
          h   (db/db-query-history s cid)]
      (expect (= 1 (count h)))
      (expect (= "What?" (:query (first h))))
      (expect (= 2 (:iterations (first h)))))))

;; =============================================================================
;; Soul/state FK integrity
;; =============================================================================

(defdescribe soul-state-integrity-test
  (it "conversation_state.conversation_soul_id points to conversation_soul.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis :title "FK test"})]
      (let [soul  (first (raw-query s {:select [:id] :from :conversation_soul}))
            state (first (raw-query s {:select [:conversation_soul_id] :from :conversation_state}))]
        (expect (= (:id soul) (:conversation_soul_id state))))))

  (it "query_soul.conversation_state_id points to conversation_state.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          _   (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (let [state (first (raw-query s {:select [:id] :from :conversation_state}))
            qsoul (first (raw-query s {:select [:conversation_state_id] :from :query_soul}))]
        (expect (= (:id state) (:conversation_state_id qsoul))))))

  (it "query_state.query_soul_id points to query_soul.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          _   (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (let [qsoul  (first (raw-query s {:select [:id] :from :query_soul}))
            qstate (first (raw-query s {:select [:query_soul_id] :from :query_state}))]
        (expect (= (:id qsoul) (:query_soul_id qstate))))))

  (it "iteration.query_state_id points to query_state.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0})]
      (let [qstate (first (raw-query s {:select [:id] :from :query_state}))
            iter   (first (raw-query s {:select [:query_state_id] :from :iteration}))]
        (expect (= (:id qstate) (:query_state_id iter))))))

  (it "expression_soul.conversation_state_id points to conversation_state.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "1" :result 1}] :duration-ms 0})]
      (let [state (first (raw-query s {:select [:id] :from :conversation_state}))
            esoul (first (raw-query s {:select [:conversation_state_id] :from :expression_soul}))]
        (expect (= (:id state) (:conversation_state_id esoul))))))

  (it "expression_state.expression_soul_id points to expression_soul.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "1" :result 1}] :duration-ms 0})]
      (let [esoul  (first (raw-query s {:select [:id] :from :expression_soul}))
            estate (first (raw-query s {:select [:expression_soul_id] :from :expression_state}))]
        (expect (= (:id esoul) (:expression_soul_id estate))))))

  (it "expression_state.iteration_id points to iteration.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "1" :result 1}] :duration-ms 0})]
      (let [iter   (first (raw-query s {:select [:id] :from :iteration}))
            estate (first (raw-query s {:select [:iteration_id] :from :expression_state}))]
        (expect (= (:id iter) (:iteration_id estate))))))

  (it "retry query_state.forked_from_query_state_id points to previous query_state.id"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})]
      (db/update-query! s qid {:status :error})
      (db/retry-query! s qid {:status :running :model "claude-4"})
      (let [states (raw-query s {:select [:id :version :forked_from_query_state_id]
                                 :from :query_state :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:forked_from_query_state_id (first states))))
        (expect (= (:id (first states)) (:forked_from_query_state_id (second states)))))))

  (it "fork conversation_state.parent_state_id points to previous state"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})]
      (db/fork-conversation! s cid {:title "fork"})
      (let [states (raw-query s {:select [:id :version :parent_state_id]
                                 :from :conversation_state :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:parent_state_id (first states))))
        (expect (= (:id (first states)) (:parent_state_id (second states)))))))

  (it "expression_soul kind and state_mode are correct for calls vs vars"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid
                                      :expressions [{:code "(+ 1 1)" :result 2}]
                                      :duration-ms 0
                                      :vars [{:name "x" :value 42 :code "(def x 42)"}]})]
      (let [souls (raw-query s {:select [:kind :state_mode :name] :from :expression_soul
                                :order-by [[:kind :asc]]})]
        (expect (= 2 (count souls)))
        ;; call comes first alphabetically
        (expect (= "call" (:kind (first souls))))
        (expect (= "stateless" (:state_mode (first souls))))
        (expect (nil? (:name (first souls))))
        ;; var
        (expect (= "var" (:kind (second souls))))
        (expect (= "stateful" (:state_mode (second souls))))
        (expect (= "x" (:name (second souls))))))))

;; =============================================================================
;; System var versioning across iterations within a query
;; =============================================================================

(defdescribe system-var-versioning-test
  (it "*reasoning* gets a new version each iteration, all under same soul"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "Explain monads" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "(+ 1 1)" :result 2}]
                                      :duration-ms 100
                                      :vars [{:name "*reasoning*" :value "First I need to understand what a monad is" :code ";; SYSTEM var"}
                                             {:name "*query*" :value "Explain monads" :code ";; SYSTEM var"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [{:code "(str \"A monad is\")" :result "A monad is"}]
                                      :duration-ms 200
                                      :vars [{:name "*reasoning*" :value "Now I can explain: a monad wraps computation" :code ";; SYSTEM var"}]})
          _   (db/store-iteration! s {:query-id qid :expressions []
                                      :duration-ms 50
                                      :vars [{:name "*reasoning*" :value "Final check: the explanation covers functor, applicative, monad" :code ";; SYSTEM var"}
                                             {:name "*answer*" :value "A monad is a design pattern..." :code ";; SYSTEM var"}]})]
      ;; Only 1 expression_soul for *reasoning* (reused across iterations)
      (expect (= 1 (raw-count s :expression_soul [:and [:= :kind "var"] [:= :name "*reasoning*"]])))
      ;; 3 versions of *reasoning*
      (let [history (db/db-var-history s cid '*reasoning*)]
        (expect (= 3 (count history)))
        (expect (= [0 1 2] (mapv :version history)))
        (expect (= "First I need to understand what a monad is" (:value (nth history 0))))
        (expect (= "Now I can explain: a monad wraps computation" (:value (nth history 1))))
        (expect (= "Final check: the explanation covers functor, applicative, monad" (:value (nth history 2)))))
      ;; *query* has only 1 version (set once on first iteration)
      (let [qh (db/db-var-history s cid '*query*)]
        (expect (= 1 (count qh)))
        (expect (= "Explain monads" (:value (first qh)))))
      ;; *answer* has only 1 version (set on final iteration)
      (let [ah (db/db-var-history s cid '*answer*)]
        (expect (= 1 (count ah)))
        (expect (= "A monad is a design pattern..." (:value (first ah)))))))

  (it "latest var registry returns max version for each system var"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "test" :status :running})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*query*" :value "test" :code ";; SYSTEM"}
                                             {:name "*reasoning*" :value "step 1" :code ";; SYSTEM"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*reasoning*" :value "step 2" :code ";; SYSTEM"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*reasoning*" :value "step 3" :code ";; SYSTEM"}
                                             {:name "*answer*" :value "42" :code ";; SYSTEM"}]})
          reg (db/db-latest-var-registry s cid)]
      (expect (= "test" (:value (get reg '*query*))))
      (expect (= 0 (:version (get reg '*query*))))
      (expect (= "step 3" (:value (get reg '*reasoning*))))
      (expect (= 2 (:version (get reg '*reasoning*))))
      (expect (= "42" (:value (get reg '*answer*))))
      (expect (= 0 (:version (get reg '*answer*))))))

  (it "user vars and system vars coexist, each with independent version chains"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "compute" :status :running})
          _   (db/store-iteration! s {:query-id qid
                                      :expressions [{:code "(def data [1 2 3])" :result [1 2 3]}]
                                      :duration-ms 10
                                      :vars [{:name "data" :value [1 2 3] :code "(def data [1 2 3])"}
                                             {:name "*query*" :value "compute" :code ";; SYSTEM"}
                                             {:name "*reasoning*" :value "I need to sum the data" :code ";; SYSTEM"}]})
          _   (db/store-iteration! s {:query-id qid
                                      :expressions [{:code "(def result (reduce + data))" :result 6}]
                                      :duration-ms 5
                                      :vars [{:name "result" :value 6 :code "(def result (reduce + data))"}
                                             {:name "*reasoning*" :value "Sum is 6, done" :code ";; SYSTEM"}
                                             {:name "*answer*" :value "6" :code ";; SYSTEM"}]})
          reg (db/db-latest-var-registry s cid)]
      ;; 5 distinct var souls
      (expect (= 5 (count reg)))
      ;; User vars
      (expect (= [1 2 3] (:value (get reg 'data))))
      (expect (= 0 (:version (get reg 'data))))
      (expect (= 6 (:value (get reg 'result))))
      (expect (= 0 (:version (get reg 'result))))
      ;; System vars
      (expect (= "compute" (:value (get reg '*query*))))
      (expect (= "Sum is 6, done" (:value (get reg '*reasoning*))))
      (expect (= 1 (:version (get reg '*reasoning*))))
      (expect (= "6" (:value (get reg '*answer*))))))

  (it "system vars version across multiple queries in same conversation"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          ;; Turn 1
          q1  (db/store-query! s {:parent-conversation-id cid :query "What is 2+2?" :status :done})
          _   (db/store-iteration! s {:query-id q1 :expressions [] :duration-ms 0
                                      :vars [{:name "*query*" :value "What is 2+2?" :code ";; SYSTEM"}
                                             {:name "*reasoning*" :value "Simple math" :code ";; SYSTEM"}
                                             {:name "*answer*" :value "4" :code ";; SYSTEM"}]})
          ;; Turn 2
          q2  (db/store-query! s {:parent-conversation-id cid :query "And 3+3?" :status :done})
          _   (db/store-iteration! s {:query-id q2 :expressions [] :duration-ms 0
                                      :vars [{:name "*query*" :value "And 3+3?" :code ";; SYSTEM"}
                                             {:name "*reasoning*" :value "Another simple one" :code ";; SYSTEM"}
                                             {:name "*answer*" :value "6" :code ";; SYSTEM"}]})
          reg (db/db-latest-var-registry s cid)]
      ;; Each system var should have version 1 (v0 from turn 1, v1 from turn 2)
      (expect (= "And 3+3?" (:value (get reg '*query*))))
      (expect (= 1 (:version (get reg '*query*))))
      (expect (= "Another simple one" (:value (get reg '*reasoning*))))
      (expect (= 1 (:version (get reg '*reasoning*))))
      (expect (= "6" (:value (get reg '*answer*))))
      (expect (= 1 (:version (get reg '*answer*))))
      ;; Full history for *answer* shows both turns
      (let [h (db/db-var-history s cid '*answer*)]
        (expect (= 2 (count h)))
        (expect (= ["4" "6"] (mapv :value h)))))))

;; =============================================================================
;; Answer lifecycle
;; =============================================================================

(defdescribe answer-lifecycle-test
  (it "query_state metadata stores answer on update"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "2+2?" :status :running})]
      ;; Before update — no answer in metadata
      (let [q (first (db/db-list-conversation-queries s cid))]
        (expect (= :running (:status q)))
        (expect (nil? (:answer q))))
      ;; After update — answer present
      (db/update-query! s qid {:answer "4" :status :success :iterations 1 :duration-ms 500})
      (let [q   (first (db/db-list-conversation-queries s cid))
            raw (first (raw-query s {:select [:metadata] :from :query_state}))]
        (expect (= :done (:status q)))
        (expect (= "4" (:answer q)))
        ;; Verify it's in the JSON metadata, not pr-str'd
        (expect (clojure.string/includes? (:metadata raw) "\"answer\":\"4\"")))))

  (it "*answer* var tracks across turns"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          ;; Turn 1: answer is 4
          q1  (db/store-query! s {:parent-conversation-id cid :query "2+2?" :status :done})
          _   (db/store-iteration! s {:query-id q1 :expressions [{:code "(+ 2 2)" :result 4}]
                                      :duration-ms 100 :answer "4"
                                      :vars [{:name "*answer*" :value "4" :code ";; SYSTEM"}]})
          _   (db/update-query! s q1 {:answer "4" :status :success :iterations 1})
          ;; Turn 2: answer changes to 6
          q2  (db/store-query! s {:parent-conversation-id cid :query "3+3?" :status :done})
          _   (db/store-iteration! s {:query-id q2 :expressions [{:code "(+ 3 3)" :result 6}]
                                      :duration-ms 80 :answer "6"
                                      :vars [{:name "*answer*" :value "6" :code ";; SYSTEM"}]})
          _   (db/update-query! s q2 {:answer "6" :status :success :iterations 1})
          ;; Turn 3: answer changes to 10
          q3  (db/store-query! s {:parent-conversation-id cid :query "5+5?" :status :done})
          _   (db/store-iteration! s {:query-id q3 :expressions [{:code "(+ 5 5)" :result 10}]
                                      :duration-ms 60 :answer "10"
                                      :vars [{:name "*answer*" :value "10" :code ";; SYSTEM"}]})
          _   (db/update-query! s q3 {:answer "10" :status :success :iterations 1})]
      ;; Latest registry shows final answer
      (let [reg (db/db-latest-var-registry s cid)]
        (expect (= "10" (:value (get reg '*answer*))))
        (expect (= 2 (:version (get reg '*answer*)))))
      ;; Full history shows all 3 answers in order
      (let [h (db/db-var-history s cid '*answer*)]
        (expect (= 3 (count h)))
        (expect (= ["4" "6" "10"] (mapv :value h)))
        (expect (= [0 1 2] (mapv :version h))))
      ;; Each query_state has its own answer in metadata
      (let [states (raw-query s {:select [:qs.query :qst.metadata]
                                 :from [[:query_soul :qs]]
                                 :join [[:query_state :qst] [:= :qst.query_soul_id :qs.id]]
                                 :order-by [[:qs.created_at :asc]]})]
        (expect (= 3 (count states)))
        (expect (clojure.string/includes? (:metadata (nth states 0)) "\"4\""))
        (expect (clojure.string/includes? (:metadata (nth states 1)) "\"6\""))
        (expect (clojure.string/includes? (:metadata (nth states 2)) "\"10\""))))))

;; =============================================================================
;; Restore — dependency chains, topological order, sandbox reconstruction
;; =============================================================================

(defdescribe restore-test
  (it "restores vars in topological order — no dependencies"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "x" :value 42 :code "(def x 42)"}
                                             {:name "y" :value "hello" :code "(def y \"hello\")"}
                                             {:name "z" :value [1 2 3] :code "(def z [1 2 3])"}]})
          restored (db/db-restore-expressions s cid)]
      (expect (= 3 (count restored)))
      (expect (= #{"x" "y" "z"} (set (map :name restored))))
      ;; All have data values
      (expect (= 42 (:result (first (filter #(= "x" (:name %)) restored)))))
      (expect (= "hello" (:result (first (filter #(= "y" (:name %)) restored)))))
      (expect (= [1 2 3] (:result (first (filter #(= "z" (:name %)) restored)))))
      ;; No dependencies
      (expect (every? #(empty? (:depends-on %)) restored))))

  (it "restores fn vars with {:rlm/ref :expr}"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "double-it" :value (fn [x] (* x 2))
                                              :code "(defn double-it [x] (* x 2))"}]})
          restored (db/db-restore-expressions s cid)
          entry    (first restored)]
      (expect (= "double-it" (:name entry)))
      (expect (= {:rlm/ref :expr} (:result entry)))
      (expect (= "(defn double-it [x] (* x 2))" (:expr entry)))))

  (it "linear dependency chain A → B → C restored in correct order"
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          ;; Iteration 1: define base-rate
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "base-rate" :value 0.05 :code "(def base-rate 0.05)"}]})
          ;; Iteration 2: define calc-interest (depends on base-rate)
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "calc-interest" :value (fn [p] p)
                                                 :code "(defn calc-interest [principal] (* principal base-rate))"}]})
          ;; Iteration 3: define monthly-payment (depends on calc-interest)
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "monthly-payment" :value (fn [p] p)
                                                 :code "(defn monthly-payment [principal] (/ (calc-interest principal) 12))"}]})
          ;; Now wire the dependencies
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul
                                 :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; base-rate → calc-interest
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "calc-interest")
                               :upstream-soul-id   (soul-by "base-rate")})
      ;; calc-interest → monthly-payment
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "monthly-payment")
                               :upstream-soul-id   (soul-by "calc-interest")})
      (let [restored (db/db-restore-expressions s cid)
            names    (mapv :name restored)]
        (expect (= 3 (count restored)))
        ;; base-rate MUST come before calc-interest, calc-interest before monthly-payment
        (expect (< (.indexOf names "base-rate") (.indexOf names "calc-interest")))
        (expect (< (.indexOf names "calc-interest") (.indexOf names "monthly-payment")))
        ;; base-rate has data, the fns have :rlm/ref :expr
        (expect (= 0.05 (:result (first restored))))
        (expect (= {:rlm/ref :expr} (:result (second restored))))
        (expect (= {:rlm/ref :expr} (:result (nth restored 2))))
        ;; Dependency metadata is correct
        (let [calc (first (filter #(= "calc-interest" (:name %)) restored))]
          (expect (= [(soul-by "base-rate")] (:depends-on calc)))
          (expect (= [(soul-by "monthly-payment")] (:depended-by calc)))))))

  (it "diamond dependency: D depends on B and C, both depend on A"
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "config" :value {:rate 0.1} :code "(def config {:rate 0.1})"}
                                                {:name "tax-fn" :value (fn [x] x) :code "(defn tax-fn [amount] (* amount (:rate config)))"}
                                                {:name "fee-fn" :value (fn [x] x) :code "(defn fee-fn [amount] (+ 10 (* amount (:rate config))))"}
                                                {:name "total-fn" :value (fn [x] x) :code "(defn total-fn [amount] (+ (tax-fn amount) (fee-fn amount)))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; config → tax-fn, config → fee-fn
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "tax-fn")
                               :upstream-soul-id   (soul-by "config")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "fee-fn")
                               :upstream-soul-id   (soul-by "config")})
      ;; tax-fn → total-fn, fee-fn → total-fn
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "total-fn")
                               :upstream-soul-id   (soul-by "tax-fn")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "total-fn")
                               :upstream-soul-id   (soul-by "fee-fn")})
      (let [restored (db/db-restore-expressions s cid)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 4 (count restored)))
        ;; config must come before tax-fn AND fee-fn
        (expect (< (idx "config") (idx "tax-fn")))
        (expect (< (idx "config") (idx "fee-fn")))
        ;; tax-fn and fee-fn must come before total-fn
        (expect (< (idx "tax-fn") (idx "total-fn")))
        (expect (< (idx "fee-fn") (idx "total-fn")))
        ;; config is data, rest are fn refs
        (expect (= {:rate 0.1} (:result (nth restored (idx "config")))))
        (expect (= {:rlm/ref :expr} (:result (nth restored (idx "total-fn")))))
        ;; total-fn depends on both tax-fn and fee-fn
        (let [total (nth restored (idx "total-fn"))]
          (expect (= 2 (count (:depends-on total))))
          (expect (= #{(soul-by "tax-fn") (soul-by "fee-fn")} (set (:depends-on total))))))))

  (it "deep chain: 5 levels deep, each depending on previous"
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          var-names ["level-0" "level-1" "level-2" "level-3" "level-4"]
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars (mapv (fn [n] {:name n :value (fn [x] x)
                                                              :code (str "(defn " n " [x] x)")}) var-names)})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; Chain: level-0 → level-1 → level-2 → level-3 → level-4
      (doseq [i (range 4)]
        (db/store-dependency! s {:conversation-state-id state-id
                                 :downstream-soul-id (soul-by (var-names (inc i)))
                                 :upstream-soul-id   (soul-by (var-names i))}))
      (let [restored (db/db-restore-expressions s cid)
            names    (mapv :name restored)]
        (expect (= 5 (count restored)))
        ;; Strict order: level-0, level-1, level-2, level-3, level-4
        (expect (= var-names names)))))

  (it "mixed data + fn vars with system vars, all restored correctly"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "analyze data" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*query*" :value "analyze data" :code ";; SYSTEM"}
                                             {:name "*reasoning*" :value "step 1" :code ";; SYSTEM"}
                                             {:name "dataset" :value [{:x 1 :y 2} {:x 3 :y 4}]
                                              :code "(def dataset [{:x 1 :y 2} {:x 3 :y 4}])"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*reasoning*" :value "step 2" :code ";; SYSTEM"}
                                             {:name "summarize" :value (fn [ds] ds)
                                              :code "(defn summarize [ds] (map :x ds))"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*reasoning*" :value "step 3" :code ";; SYSTEM"}
                                             {:name "*answer*" :value "[1 3]" :code ";; SYSTEM"}
                                             {:name "result" :value [1 3]
                                              :code "(def result (summarize dataset))"}]})
          ;; Wire: dataset → summarize (it reads dataset), dataset + summarize → result
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "summarize")
                               :upstream-soul-id   (soul-by "dataset")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "result")
                               :upstream-soul-id   (soul-by "dataset")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "result")
                               :upstream-soul-id   (soul-by "summarize")})
      (let [restored (db/db-restore-expressions s cid)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)]
        ;; All 6 vars present (3 system + 3 user)
        (expect (= 6 (count restored)))
        ;; System vars have data values, latest versions
        (expect (= "analyze data" (:result (by-name "*query*"))))
        (expect (= "step 3" (:result (by-name "*reasoning*"))))
        (expect (= "[1 3]" (:result (by-name "*answer*"))))
        ;; dataset is data
        (expect (= [{:x 1 :y 2} {:x 3 :y 4}] (:result (by-name "dataset"))))
        ;; summarize is fn ref
        (expect (= {:rlm/ref :expr} (:result (by-name "summarize"))))
        ;; result is data (was computed)
        (expect (= [1 3] (:result (by-name "result"))))
        ;; Topological order: dataset before summarize, both before result
        (expect (< (idx "dataset") (idx "summarize")))
        (expect (< (idx "dataset") (idx "result")))
        (expect (< (idx "summarize") (idx "result")))))))

;; =============================================================================
;; Sophisticated dependency patterns
;; =============================================================================

(defdescribe advanced-dependency-test
  (it "higher-order fn: make-adder returns a fn, result bound as var"
    ;; (defn make-adder [n] (fn [x] (+ x n)))
    ;; (def add-10 (make-adder 10))
    ;; (def result (add-10 5))  => 15
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "make-adder" :value (fn [n] (fn [x] (+ x n)))
                                                 :code "(defn make-adder [n] (fn [x] (+ x n)))"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "add-10" :value (fn [x] (+ x 10))
                                                 :code "(def add-10 (make-adder 10))"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "result" :value 15
                                                 :code "(def result (add-10 5))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; make-adder -> add-10 -> result
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "add-10")
                               :upstream-soul-id   (soul-by "make-adder")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "result")
                               :upstream-soul-id   (soul-by "add-10")})
      (let [restored (db/db-restore-expressions s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; Topological: make-adder < add-10 < result
        (expect (< (idx "make-adder") (idx "add-10")))
        (expect (< (idx "add-10") (idx "result")))
        ;; make-adder is a fn -> ref
        (expect (= {:rlm/ref :expr} (:result (by-name "make-adder"))))
        ;; add-10 is ALSO a fn (returned by make-adder) -> ref
        (expect (= {:rlm/ref :expr} (:result (by-name "add-10"))))
        ;; result is data (15)
        (expect (= 15 (:result (by-name "result")))))))

  (it "closure chain: factory -> instance -> bound literal via eval"
    ;; (def config {:multiplier 3})
    ;; (defn make-scaler [] (let [m (:multiplier config)] (fn [x] (* x m))))
    ;; (def scale (make-scaler))
    ;; (def scaled-data (mapv scale [1 2 3 4 5]))
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "config" :value {:multiplier 3}
                                                 :code "(def config {:multiplier 3})"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "make-scaler" :value (fn [] (fn [x] (* x 3)))
                                                 :code "(defn make-scaler [] (let [m (:multiplier config)] (fn [x] (* x m))))"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "scale" :value (fn [x] (* x 3))
                                                 :code "(def scale (make-scaler))"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "scaled-data" :value [3 6 9 12 15]
                                                 :code "(def scaled-data (mapv scale [1 2 3 4 5]))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; config -> make-scaler -> scale -> scaled-data
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "make-scaler")
                               :upstream-soul-id   (soul-by "config")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "scale")
                               :upstream-soul-id   (soul-by "make-scaler")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "scaled-data")
                               :upstream-soul-id   (soul-by "scale")})
      (let [restored (db/db-restore-expressions s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 4 (count restored)))
        ;; Strict topological order
        (expect (< (idx "config") (idx "make-scaler")))
        (expect (< (idx "make-scaler") (idx "scale")))
        (expect (< (idx "scale") (idx "scaled-data")))
        ;; config = data, make-scaler = fn ref, scale = fn ref, scaled-data = data
        (expect (= {:multiplier 3} (:result (by-name "config"))))
        (expect (= {:rlm/ref :expr} (:result (by-name "make-scaler"))))
        (expect (= {:rlm/ref :expr} (:result (by-name "scale"))))
        (expect (= [3 6 9 12 15] (:result (by-name "scaled-data")))))))

  (it "multi-version var with dependency: changing upstream propagates ref"
    ;; Iter 1: (def base 10)
    ;; Iter 2: (defn compute [x] (+ x base))
    ;; Iter 3: (def base 20)  -- base changes!
    ;; Iter 4: (def answer (compute 5)) -- should use new base
    ;; At restore time, base=20 (latest version), compute has :rlm/ref :expr
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "base" :value 10 :code "(def base 10)"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "compute" :value (fn [x] (+ x 10))
                                                 :code "(defn compute [x] (+ x base))"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "base" :value 20 :code "(def base 20)"}]})
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars [{:name "answer" :value 25
                                                 :code "(def answer (compute 5))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; base -> compute, compute -> answer, base -> answer (transitive)
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "compute")
                               :upstream-soul-id   (soul-by "base")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "answer")
                               :upstream-soul-id   (soul-by "compute")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "answer")
                               :upstream-soul-id   (soul-by "base")})
      (let [restored (db/db-restore-expressions s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; base LATEST is 20 (version 1), not 10
        (expect (= 20 (:result (by-name "base"))))
        (expect (= 1 (:version (by-name "base"))))
        ;; compute is fn ref, needs re-eval with new base
        (expect (= {:rlm/ref :expr} (:result (by-name "compute"))))
        (expect (= "(defn compute [x] (+ x base))" (:expr (by-name "compute"))))
        ;; answer is data
        (expect (= 25 (:result (by-name "answer"))))
        ;; Order: base before compute before answer
        (expect (< (idx "base") (idx "compute")))
        (expect (< (idx "compute") (idx "answer"))))))

  (it "wide fan-out: one config feeds 5 independent fns"
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          qid    (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          fn-names ["fn-a" "fn-b" "fn-c" "fn-d" "fn-e"]
          _      (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                         :vars (into [{:name "shared-cfg" :value {:k 1}
                                                       :code "(def shared-cfg {:k 1})"}]
                                                 (mapv (fn [n] {:name n :value (fn [x] x)
                                                                :code (str "(defn " n " [x] (+ x (:k shared-cfg)))")}) fn-names))})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; shared-cfg -> each fn
      (doseq [n fn-names]
        (db/store-dependency! s {:conversation-state-id state-id
                                 :downstream-soul-id (soul-by n)
                                 :upstream-soul-id   (soul-by "shared-cfg")}))
      (let [restored (db/db-restore-expressions s cid)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 6 (count restored)))
        ;; shared-cfg must be first (all fns depend on it)
        (expect (= 0 (idx "shared-cfg")))
        ;; All fns come after
        (doseq [n fn-names]
          (expect (< (idx "shared-cfg") (idx n)))))))

  (it "cross-query dependency: var from turn 1 used by fn in turn 2"
    (let [s      (store)
          cid    (db/store-conversation! s {:channel :vis})
          ;; Turn 1: define data
          q1     (db/store-query! s {:parent-conversation-id cid :query "load data" :status :done})
          _      (db/store-iteration! s {:query-id q1 :expressions [] :duration-ms 0
                                         :vars [{:name "raw-data" :value [10 20 30]
                                                 :code "(def raw-data [10 20 30])"}]})
          ;; Turn 2: define fn + compute result using data from turn 1
          q2     (db/store-query! s {:parent-conversation-id cid :query "process it" :status :done})
          _      (db/store-iteration! s {:query-id q2 :expressions [] :duration-ms 0
                                         :vars [{:name "avg-fn" :value (fn [xs] (/ (reduce + xs) (count xs)))
                                                 :code "(defn avg-fn [xs] (/ (reduce + xs) (count xs)))"}]})
          _      (db/store-iteration! s {:query-id q2 :expressions [] :duration-ms 0
                                         :vars [{:name "average" :value 20
                                                 :code "(def average (avg-fn raw-data))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; raw-data -> avg-fn (reads it), raw-data -> average, avg-fn -> average
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "avg-fn")
                               :upstream-soul-id   (soul-by "raw-data")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "average")
                               :upstream-soul-id   (soul-by "raw-data")})
      (db/store-dependency! s {:conversation-state-id state-id
                               :downstream-soul-id (soul-by "average")
                               :upstream-soul-id   (soul-by "avg-fn")})
      (let [restored (db/db-restore-expressions s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; raw-data from turn 1 is still data
        (expect (= [10 20 30] (:result (by-name "raw-data"))))
        ;; avg-fn is fn ref
        (expect (= {:rlm/ref :expr} (:result (by-name "avg-fn"))))
        ;; average is computed data
        (expect (= 20 (:result (by-name "average"))))
        ;; Order: raw-data before avg-fn before average
        (expect (< (idx "raw-data") (idx "avg-fn")))
        (expect (< (idx "avg-fn") (idx "average")))))))

;; =============================================================================
;; Lazy seq / infinite range safety
;; =============================================================================

(defdescribe lazy-seq-safety-test
  (it "infinite range → {:rlm/ref :expr} — never realized"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "nums" :value (range) :code "(def nums (range))"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      ;; Must not hang!
      (expect (= {:rlm/ref :expr} (:value v)))
      (expect (= "(def nums (range))" (:code v)))))

  (it "infinite repeat → {:rlm/ref :expr}"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "ones" :value (repeat 1) :code "(def ones (repeat 1))"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      (expect (= {:rlm/ref :expr} (:value v)))))

  (it "iterate → {:rlm/ref :expr}"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "nats" :value (iterate inc 0)
                                              :code "(def nats (iterate inc 0))"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      (expect (= {:rlm/ref :expr} (:value v)))))

  (it "small lazy seq (map inc [1 2 3]) → {:rlm/ref :expr} — lazy is lazy"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "small" :value (map inc [1 2 3])
                                              :code "(def small (map inc [1 2 3]))"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      ;; Even small lazy seqs are refs — they're computations, not data
      (expect (= {:rlm/ref :expr} (:value v)))))

  (it "realized vector is stored as data"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "v" :value [1 2 3 4 5]
                                              :code "(def v [1 2 3 4 5])"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      (expect (= [1 2 3 4 5] (:value v)))))

  (it "realized list is stored as data"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "l" :value '(1 2 3)
                                              :code "(def l '(1 2 3))"}]})
          v   (first (db/db-list-iteration-vars s iid))]
      (expect (= '(1 2 3) (:value v)))))

  (it "lazy seq inside a map → map stored, lazy value becomes ref"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          iid (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "mixed" :value {:data [1 2 3]
                                                                    :lazy (map inc [10 20])
                                                                    :infinite (range)}
                                              :code "(def mixed {...})"}]})
          m   (:value (first (db/db-list-iteration-vars s iid)))]
      (expect (map? m))
      (expect (= [1 2 3] (:data m)))
      ;; Both lazy seqs → ref, regardless of size
      (expect (= {:rlm/ref :expr} (:lazy m)))
      (expect (= {:rlm/ref :expr} (:infinite m)))))

  (it "lazy seq as expression result → ref"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :running})
          _   (db/store-iteration! s {:query-id qid
                                      :expressions [{:code "(range)" :result (range)}
                                                    {:code "(vec (range 5))" :result [0 1 2 3 4]}]
                                      :duration-ms 5})
          rows (raw-query s {:select [:est.result :est.expr]
                            :from [[:expression_state :est]]
                            :join [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
                            :where [:= :es.kind "call"]
                            :order-by [[:est.created_at :asc]]})]
      ;; (range) → ref
      (expect (= {:rlm/ref :expr} (thaw-blob (:result (first rows)))))
      ;; (vec (range 5)) → realized vector, stored as data
      (expect (= [0 1 2 3 4] (thaw-blob (:result (second rows)))))))
)
;; =============================================================================
;; Integration: store → wipe sandbox → restore → use
;; =============================================================================

(defdescribe restore-integration-test
  (it "data var: store, wipe, restore, read back"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          ;; Create SCI sandbox, def a var
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)
          _   (sci/eval-string+ sci-ctx "(def data [10 20 30])" {:ns (sci/find-ns sci-ctx 'sandbox)})
          ;; Store it
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "data" :value [10 20 30] :code "(def data [10 20 30])"}]})
          ;; Wipe: fresh sandbox (simulates disconnect)
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)]
      ;; Verify var is gone
      (expect (try (sci/eval-string+ sci-ctx "data" {:ns (sci/find-ns sci-ctx 'sandbox)}) false
                (catch Exception _ true)))
      ;; Restore
      (let [results (sci-env/restore-sandbox! sci-ctx s cid)]
        (expect (= 1 (count results)))
        (expect (true? (:success? (first results))))
        (expect (= :data (:restored-via (first results)))))
      ;; Now the var works
      (expect (= [10 20 30] (:val (sci/eval-string+ sci-ctx "data" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "function var: store, wipe, restore via eval, call it"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          ;; Store a fn (result will be {:rlm/ref :expr})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "double-it" :value (fn [x] (* x 2))
                                              :code "(defn double-it [x] (* x 2))"}]})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)]
      ;; Restore
      (let [results (sci-env/restore-sandbox! sci-ctx s cid)]
        (expect (= 1 (count results)))
        (expect (= :eval (:restored-via (first results))))
        (expect (true? (:success? (first results)))))
      ;; Call the restored function
      (expect (= 42 (:val (sci/eval-string+ sci-ctx "(double-it 21)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "dependency chain: data → fn → result, all restored in order"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          ;; Store chain: rate → calc → answer
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "rate" :value 0.1 :code "(def rate 0.1)"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "calc" :value (fn [x] (* x 0.1))
                                              :code "(defn calc [amount] (* amount rate))"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "answer" :value 100.0
                                              :code "(def answer (calc 1000))"}]})
          ;; Wire dependencies
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)
          _   (db/store-dependency! s {:conversation-state-id state-id
                                       :downstream-soul-id (soul-by "calc")
                                       :upstream-soul-id   (soul-by "rate")})
          _   (db/store-dependency! s {:conversation-state-id state-id
                                       :downstream-soul-id (soul-by "answer")
                                       :upstream-soul-id   (soul-by "calc")})
          _   (db/store-dependency! s {:conversation-state-id state-id
                                       :downstream-soul-id (soul-by "answer")
                                       :upstream-soul-id   (soul-by "rate")})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)]
      ;; Restore
      (let [results (sci-env/restore-sandbox! sci-ctx s cid)
            by-name (into {} (map (fn [r] [(:name r) r])) results)]
        ;; rate restored as data, calc as eval, answer as data
        (expect (= :data (:restored-via (by-name "rate"))))
        (expect (= :eval (:restored-via (by-name "calc"))))
        (expect (= :data (:restored-via (by-name "answer"))))
        (expect (every? :success? results)))
      ;; All work in the sandbox
      (expect (= 0.1 (:val (sci/eval-string+ sci-ctx "rate" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= 50.0 (:val (sci/eval-string+ sci-ctx "(calc 500)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= 100.0 (:val (sci/eval-string+ sci-ctx "answer" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "higher-order fn chain: factory → instance → call"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "make-adder" :value (fn [n] (fn [x] (+ x n)))
                                              :code "(defn make-adder [n] (fn [x] (+ x n)))"}]})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "add-5" :value (fn [x] (+ x 5))
                                              :code "(def add-5 (make-adder 5))"}]})
          ;; Wire: make-adder → add-5
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)
          _   (db/store-dependency! s {:conversation-state-id state-id
                                       :downstream-soul-id (soul-by "add-5")
                                       :upstream-soul-id   (soul-by "make-adder")})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)]
      (sci-env/restore-sandbox! sci-ctx s cid)
      ;; make-adder works (it's a fn, restored via eval)
      (expect (= 15 (:val (sci/eval-string+ sci-ctx "((make-adder 10) 5)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      ;; add-5 works (closure created by make-adder, restored via eval)
      (expect (= 12 (:val (sci/eval-string+ sci-ctx "(add-5 7)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "system vars are restored: *query*, *reasoning*, *answer*"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})
          qid (db/store-query! s {:parent-conversation-id cid :query "x" :status :done})
          _   (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                      :vars [{:name "*query*" :value "What is 2+2?" :code ";; SYSTEM var"}
                                             {:name "*reasoning*" :value "Simple math" :code ";; SYSTEM var"}
                                             {:name "*answer*" :value "4" :code ";; SYSTEM var"}]})
          {:keys [sci-ctx]} (sci-env/create-sci-context nil)]
      (let [results (sci-env/restore-sandbox! sci-ctx s cid)
            by-name (into {} (map (fn [r] [(:name r) r])) results)]
        ;; SYSTEM vars with ;; SYSTEM var code are data-restored
        (expect (= :data (:restored-via (by-name "*query*"))))
        (expect (= :data (:restored-via (by-name "*reasoning*"))))
        (expect (= :data (:restored-via (by-name "*answer*"))))
        (expect (every? :success? results)))
      (expect (= "What is 2+2?" (:val (sci/eval-string+ sci-ctx "*query*" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= "4" (:val (sci/eval-string+ sci-ctx "*answer*" {:ns (sci/find-ns sci-ctx 'sandbox)})))))))

;; =============================================================================
;; Log
;; =============================================================================

(defdescribe log-test
  (it "inserts into log table with FK scope"
    (let [s   (store)
          cid (db/store-conversation! s {:channel :vis})]
      (db/log! s {:level :info :event "test.event" :data "{\"k\":1}"
                  :conversation-soul-id (second cid)})
      (expect (= 1 (raw-count s :log)))
      (let [row (first (raw-query s {:select [:*] :from :log}))]
        (expect (= "info" (:level row)))
        (expect (= "test.event" (:event row)))
        (expect (= (str (second cid)) (:conversation_soul_id row)))))))
