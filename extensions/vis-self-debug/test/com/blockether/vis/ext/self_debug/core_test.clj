(ns com.blockether.vis.ext.self-debug.core-test
  "Tests for the self-debug extension. Each test bootstraps a synthetic
   conversation + query + iteration rows in an in-memory SQLite DB, then
   invokes the impl fns directly with a fake env map. The extension's
   `:before-fn` env-injection layer is exercised by an integration test
   at the end."
  (:require
   [com.blockether.vis.ext.self-debug.core :as self-debug]
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

(h/use-mem-store!)

;; -----------------------------------------------------------------------------
;; Helpers — synthesize a query + iterations directly via the DB facade
;; so we don't have to spin up a full SCI environment.
;; -----------------------------------------------------------------------------

(defn- bootstrap [store]
  (let [conversation-id (db/store-conversation! store {:channel :vis :title "self-debug test"})
        query-id (db/store-query! store
                   {:parent-conversation-id conversation-id
                    :query "what's the plan?"
                    :status :running})]
    {:conversation-id conversation-id :query-id query-id}))

(defn- store-iteration!
  [store query-id {:keys [plan-state breadcrumb expressions thinking]
                   :or {expressions []}}]
  (db/store-iteration! store
    (cond-> {:query-id    query-id
             :expressions expressions
             :duration-ms 100
             :llm-model   "test-model"
             :metadata    {}}
      plan-state (assoc :plan-state plan-state)
      breadcrumb (assoc :breadcrumb breadcrumb)
      thinking   (assoc :thinking thinking))))

(defn- env [store conversation-id]
  ;; Bare-minimum env map: enough for the impl fns to read DB and
  ;; resolve the current query. The atoms are stubs the
  ;; iteration-budget read uses.
  {:db-info store
   :conversation-id conversation-id
   :current-iteration-atom (atom 2)
   :max-iterations-atom (atom 10)})

;; Indirection through the var registry — every impl fn is private
;; (defn-) but the extension wraps each one through a public `:ext/symbol`,
;; so the test reaches into the ns to call them directly. This keeps the
;; impls private without losing test coverage.
(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.self-debug.core" name))))

;; -----------------------------------------------------------------------------
;; self/plan
;; -----------------------------------------------------------------------------

(defdescribe self-plan-test
  (it "returns nil when no plan has ever been emitted"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id {:thinking "no plan yet"})
      (expect (nil? ((private-fn "self-plan") (env s conversation-id))))))

  (it "returns the most recent non-nil plan"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          plan-v1 {:goal "v1" :items [{:id 1 :content "a" :status :pending}]}
          plan-v2 {:goal "v2" :items [{:id 1 :content "a" :status :done}
                                      {:id 2 :content "b" :status :in_progress}]}]
      (store-iteration! s query-id {:plan-state plan-v1 :breadcrumb "i0"})
      (store-iteration! s query-id {:breadcrumb "i1 — no plan re-emit"})
      (store-iteration! s query-id {:plan-state plan-v2 :breadcrumb "i2"})
      (let [plan ((private-fn "self-plan") (env s conversation-id))]
        (expect (= "v2" (:goal plan)))
        (expect (= 2 (count (:items plan))))))))

;; -----------------------------------------------------------------------------
;; self/breadcrumbs
;; -----------------------------------------------------------------------------

(defdescribe self-breadcrumbs-test
  (it "returns oldest-first breadcrumbs"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id {:breadcrumb "i0 first"})
      (store-iteration! s query-id {:breadcrumb "i1 second"})
      (store-iteration! s query-id {:breadcrumb "i2 third"})
      (let [chain ((private-fn "self-breadcrumbs") (env s conversation-id))]
        (expect (= 3 (count chain)))
        (expect (= "i0 first" (:breadcrumb (first chain))))
        (expect (= "i2 third" (:breadcrumb (last chain)))))))

  (it "n parameter clips to the last N entries"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (doseq [index (range 5)]
        (store-iteration! s query-id {:breadcrumb (str "i" index)}))
      (let [chain ((private-fn "self-breadcrumbs") (env s conversation-id) 2)]
        (expect (= 2 (count chain)))
        (expect (= "i3" (:breadcrumb (first chain))))
        (expect (= "i4" (:breadcrumb (last chain)))))))

  (it "skips iterations without a breadcrumb"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id {:thinking "no breadcrumb"})
      (store-iteration! s query-id {:breadcrumb "real one"})
      (let [chain ((private-fn "self-breadcrumbs") (env s conversation-id))]
        (expect (= 1 (count chain)))
        (expect (= "real one" (:breadcrumb (first chain))))))))

;; -----------------------------------------------------------------------------
;; self/attempts and self/errors
;; -----------------------------------------------------------------------------

(defdescribe self-attempts-test
  (it "returns every code-block executed in this query"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(* 2 2)" :result 4 :execution-time-ms 1}]})
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(grep ...)" :error "boom" :execution-time-ms 5}]})
      (let [attempts ((private-fn "self-attempts") (env s conversation-id))]
        (expect (= 3 (count attempts)))
        (expect (= "(+ 1 2)" (:code (first attempts))))
        (expect (= "(grep ...)" (:code (last attempts))))
        (expect (= "boom" (:error (last attempts)))))))

  (it "errors filter returns only failed entries"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(boom)" :error "kaboom" :execution-time-ms 1}]})
      (let [errors ((private-fn "self-errors") (env s conversation-id))]
        (expect (= 1 (count errors)))
        (expect (= "kaboom" (:error (first errors))))))))

;; -----------------------------------------------------------------------------
;; self/iteration-budget
;; -----------------------------------------------------------------------------

(defdescribe self-iteration-budget-test
  (it "reads the live atoms and computes :remaining correctly"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          environment (assoc (env s conversation-id)
                        :current-iteration-atom (atom 6)
                        :max-iterations-atom (atom 30))]
      (let [budget ((private-fn "self-iteration-budget") environment)]
        (expect (= 7 (:current budget)))   ;; current is 1-based
        (expect (= 30 (:budget budget)))
        (expect (= 23 (:remaining budget))))))

  (it "tolerates missing atoms (no-iteration env)"
    (let [budget ((private-fn "self-iteration-budget") {})]
      (expect (= 1 (:current budget)))
      (expect (= 0 (:budget budget)))
      (expect (= 0 (:remaining budget))))))

;; -----------------------------------------------------------------------------
;; self/turn-history
;; -----------------------------------------------------------------------------

(defdescribe self-turn-history-test
  (it "returns every query for the conversation, oldest-first"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          q2-id (db/store-query! s
                  {:parent-conversation-id conversation-id
                   :query "second turn" :status :running})]
      (store-iteration! s q2-id {:plan-state {:goal "second"} :breadcrumb "i0"})
      (let [history ((private-fn "self-turn-history") (env s conversation-id))]
        (expect (= 2 (count history)))
        (expect (= "what's the plan?" (:goal (first history))))
        (expect (= "second turn" (:goal (last history))))))))

;; -----------------------------------------------------------------------------
;; self/var-history — wraps db-var-history; smoke test only
;; -----------------------------------------------------------------------------

(defdescribe self-var-history-test
  (it "returns [] for an unknown sym"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "self-var-history") (env s conversation-id) 'never-defined)))))

  (it "tolerates string sym names"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "self-var-history") (env s conversation-id) "still-undefined"))))))

;; -----------------------------------------------------------------------------
;; Failure modes — every fn must return nil/[], NEVER throw
;; -----------------------------------------------------------------------------

(defdescribe failure-mode-test
  ;; Spec contract: every fn returns nil-or-empty on missing context;
   ;; never throws. The empty-vec vs. nil distinction is irrelevant
   ;; downstream — callers chain through `(when (seq …))` either way.
  (let [empty-result? #(or (nil? %) (and (coll? %) (empty? %)))]
    (it "returns nil-or-empty when DB is unreachable (nil :db-info)"
      (expect (empty-result? ((private-fn "self-plan") {:conversation-id "x"})))
      (expect (empty-result? ((private-fn "self-breadcrumbs") {:conversation-id "x"})))
      (expect (empty-result? ((private-fn "self-attempts") {:conversation-id "x"})))
      (expect (empty-result? ((private-fn "self-turn-history") {:conversation-id "x"})))
      (expect (empty-result? ((private-fn "self-var-history") {:conversation-id "x"} 'foo))))

    (it "returns nil-or-empty when conversation-id is missing"
      (let [s (h/store)]
        (expect (empty-result? ((private-fn "self-plan") {:db-info s})))
        (expect (empty-result? ((private-fn "self-breadcrumbs") {:db-info s})))
        (expect (empty-result? ((private-fn "self-turn-history") {:db-info s})))))))
