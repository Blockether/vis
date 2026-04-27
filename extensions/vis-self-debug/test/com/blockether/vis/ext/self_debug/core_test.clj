(ns com.blockether.vis.ext.self-debug.core-test
  "Tests for the self-debug extension's consolidated API. Five
   functions, each returning a map or vector. Each test bootstraps
   synthetic conversation + query + iteration rows in an in-memory
   SQLite DB, then invokes the impl fns directly with a fake env map."
  (:require
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

;; The extension's core ns calls `register-global!` at load time;
;; required eagerly so the impl fns are interned before tests run.
(require '[com.blockether.vis.ext.self-debug.core])

(h/use-mem-store!)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- bootstrap [store]
  (let [conversation-id (db/store-conversation! store
                          {:channel :vis :title "self-debug test"})
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
  {:db-info store
   :conversation-id conversation-id
   :current-iteration-atom (atom 2)
   :max-iterations-atom (atom 10)})

;; The impl fns are private — reach via the var registry so tests
;; don't depend on a public re-export.
(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.self-debug.core" name))))

;; -----------------------------------------------------------------------------
;; (self/turn) — single rich snapshot of the current turn
;; -----------------------------------------------------------------------------

(defdescribe self-turn-test
  (it "returns nil when DB is unreachable"
    (expect (nil? ((private-fn "self-turn") {:conversation-id "x"}))))

  (it "returns a snapshot map with goal / status / iteration / cost / elapsed-ms / empty plan"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          turn ((private-fn "self-turn") (env s conversation-id))]
      (expect (= "what's the plan?" (:goal turn)))
      (expect (= :running (:status turn)))
      (expect (map? (:iteration turn)))
      (expect (= 3 (:current (:iteration turn))))
      (expect (= 10 (:budget (:iteration turn))))
      (expect (map? (:cost turn)))
      (expect (vector? (:breadcrumbs turn)))
      (expect (vector? (:attempts turn)))
      (expect (vector? (:errors turn)))
      (expect (nil? (:plan turn)))))

  (it "carries the sticky plan and the breadcrumb chain in the snapshot"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          plan {:goal "g" :items [{:id 1 :content "a" :status :in_progress}]}]
      (store-iteration! s query-id {:plan-state plan :breadcrumb "i0 first"})
      (store-iteration! s query-id {:breadcrumb "i1 second"})
      (let [turn ((private-fn "self-turn") (env s conversation-id))]
        (expect (= "g" (-> turn :plan :goal)))
        (expect (= 2 (count (:breadcrumbs turn))))
        (expect (= "i0 first" (-> turn :breadcrumbs first :breadcrumb))))))

  (it "splits attempts and errors so callers don't have to filter twice"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(boom)"  :error "boom" :execution-time-ms 1}]})
      (let [turn ((private-fn "self-turn") (env s conversation-id))]
        (expect (= 2 (count (:attempts turn))))
        (expect (= 1 (count (:errors turn))))
        (expect (= "boom" (-> turn :errors first :error))))))

  (it "surfaces the redundancy summary aggregated across iteration metadata"
    ;; The Phase 2-m metric lands in iteration.metadata as :dedup-saves
    ;; per iter; (self/turn).:redundancy aggregates across iters.
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db/store-iteration! s
        {:query-id    query-id
         :expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 0
                       :expression-redundancy-fraction 0.0}})
      (db/store-iteration! s
        {:query-id    query-id
         :expressions [{:id 0 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 1
                       :expression-redundancy-fraction 1.0}})
      (let [turn ((private-fn "self-turn") (env s conversation-id))
            redundancy (:redundancy turn)]
        (expect (= 1 (:duplicate-count redundancy)))
        (expect (= 3 (:total-count redundancy)))
        (expect (< (Math/abs (- (/ 1.0 3.0) (:fraction redundancy))) 1e-9))))))

;; -----------------------------------------------------------------------------
;; (self/conversation [id]) — current or specific conversation
;; -----------------------------------------------------------------------------

(defdescribe self-conversation-test
  (it "no-arg form returns the current conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "self-conversation") (env s conversation-id))]
      (expect (= conversation-id (:id conversation)))
      (expect (= :vis (:channel conversation)))
      (expect (vector? (:turns conversation)))
      (expect (= 1 (:turn-count conversation)))))

  (it "arg form fetches any conversation by id"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (db/store-conversation! s {:channel :telegram :title "other"})]
      (let [conversation ((private-fn "self-conversation") (env s conversation-id) other)]
        (expect (= other (:id conversation)))
        (expect (= :telegram (:channel conversation)))
        (expect (= 0 (:turn-count conversation))))))

  (it "turns include goal/outcome/answer when present"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db/update-query! s query-id
        {:answer "42" :iterations 1 :duration-ms 50 :status :done
         :prior-outcome :complete})
      (let [conversation ((private-fn "self-conversation") (env s conversation-id))
            turn (first (:turns conversation))]
        (expect (= "what's the plan?" (:goal turn)))
        (expect (= "42" (:answer turn)))
        (expect (= :complete (:outcome turn)))))))

;; -----------------------------------------------------------------------------
;; (self/conversations [channel]) — list across one or all channels
;; -----------------------------------------------------------------------------

(defdescribe self-conversations-test
  (it "no-arg form scans every known channel"
    (let [s (h/store)
          a (db/store-conversation! s {:channel :vis :title "vis-a"})
          b (db/store-conversation! s {:channel :telegram :title "tg-b"})
          all ((private-fn "self-conversations") (env s a))
          ids (set (map :id all))]
      (expect (contains? ids a))
      (expect (contains? ids b))))

  (it "channel-arg form filters to one channel"
    (let [s (h/store)
          a (db/store-conversation! s {:channel :vis :title "vis-a"})
          _ (db/store-conversation! s {:channel :telegram :title "tg-b"})
          tui-list ((private-fn "self-conversations") (env s a) :telegram)]
      (expect (= 1 (count tui-list)))
      (expect (= :telegram (:channel (first tui-list))))))

  (it "every entry carries id, channel, title, turn-count"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          all ((private-fn "self-conversations") (env s conversation-id))
          this (first (filter #(= conversation-id (:id %)) all))]
      (expect (some? this))
      (expect (= "self-debug test" (:title this)))
      (expect (= 1 (:turn-count this))))))

;; -----------------------------------------------------------------------------
;; (self/var-history sym [conv-id])
;; -----------------------------------------------------------------------------

(defdescribe self-var-history-test
  (it "returns [] for an unknown sym"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "self-var-history") (env s conversation-id) 'never-defined)))))

  (it "tolerates string sym names"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "self-var-history") (env s conversation-id) "still-undefined")))))

  (it "explicit conv-id form queries a different conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (db/store-conversation! s {:channel :vis :title "other"})]
      (expect (= [] ((private-fn "self-var-history") (env s conversation-id) 'foo other))))))

;; -----------------------------------------------------------------------------
;; (self/find-attempts pattern [conv-id])
;; -----------------------------------------------------------------------------

(defdescribe self-find-attempts-test
  (it "one-arg form regex-searches the current turn"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)"        :result 3 :execution-time-ms 1}
                       {:id 1 :code "(grep \"FOO\")" :result [] :execution-time-ms 1}
                       {:id 2 :code "(grep \"BAR\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "self-find-attempts") (env s conversation-id) "grep")]
        (expect (= 2 (count hits)))
        (expect (every? #(re-find #"grep" (:code %)) hits))
        ;; :turn-id resolves to the latest query of the conversation — a
        ;; UUID identifying the turn the attempt belongs to.
        (expect (= query-id (-> hits first :turn-id)))
        (expect (every? #(= query-id (:turn-id %)) hits)))))

  (it "accepts a Pattern object directly"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (store-iteration! s query-id
        {:expressions [{:id 0 :code "(defn foo [x] x)" :result nil :execution-time-ms 1}]})
      (let [hits ((private-fn "self-find-attempts") (env s conversation-id) #"\bdefn\b")]
        (expect (= 1 (count hits))))))

  (it "two-arg form scans every turn of the given conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          q2 (db/store-query! s
               {:parent-conversation-id conversation-id
                :query "second turn" :status :running})]
      ;; Leave q1 empty; fill q2.
      (store-iteration! s q2
        {:expressions [{:id 0 :code "(grep \"target\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "self-find-attempts") (env s conversation-id) "grep" conversation-id)]
        (expect (= 1 (count hits)))))))

;; -----------------------------------------------------------------------------
;; Failure modes — every fn must return nil/[], NEVER throw
;; -----------------------------------------------------------------------------

(defdescribe failure-mode-test
  (let [empty-result? #(or (nil? %) (and (coll? %) (empty? %)))]
    (it "returns nil-or-empty when DB is unreachable"
      (let [environment {:conversation-id "x"}]
        (expect (empty-result? ((private-fn "self-turn") environment)))
        (expect (empty-result? ((private-fn "self-conversation") environment)))
        (expect (empty-result? ((private-fn "self-conversations") environment)))
        (expect (empty-result? ((private-fn "self-var-history") environment 'foo)))
        (expect (empty-result? ((private-fn "self-find-attempts") environment "grep")))))

    (it "returns nil-or-empty when conversation-id is missing"
      (let [s (h/store)
            environment {:db-info s}]
        (expect (empty-result? ((private-fn "self-turn") environment)))
        (expect (empty-result? ((private-fn "self-conversation") environment)))))))
