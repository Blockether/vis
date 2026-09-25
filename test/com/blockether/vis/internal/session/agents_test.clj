(ns com.blockether.vis.internal.session.agents-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store! {"subagents" true})

(deftest schema-owned-agent-bounds-test
  (let [schema
        (document/schema-document "agents")

        agent
        {"session_id" "child"
         "parent_id" "parent"
         "leader_id" "parent"
         "team_id" "task"
         "task" "Inspect the existing tests"
         "status" "queued"
         "depth" 1
         "iteration_budget" 32
         "iterations_used" 0}]

    (is (= agents/default-iterations
           (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"])
           32))
    (is (= agents/max-depth (get-in schema ["$defs" "agent" "properties" "depth" "maximum"]) 2))
    (is (= agents/max-team-children (get schema "x-vis-max-team-children") 32))
    (is (= agents/max-active-children (get schema "x-vis-max-active-children") 8))
    (is (document/valid? "agents" agent))
    (is (not (document/valid? "agents" (assoc agent "depth" 3))))
    (is (not (document/valid? "agents" (assoc agent "iteration_budget" 201))))
    (is (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8192 "x"))}))
    (is (not (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8193 "x"))})))))

(defn child!
  [db parent opts]
  (let [turn
        (ps/db-store-session-turn!
          db
          {:parent-session-id parent :user-request "Parent task" :status :running})

        ancestor
        (agents/info db parent)]

    (str (h/fork-session-at-turn! db
                                  parent
                                  {:through-turn-id turn
                                   :agent (merge {:parent_id (str parent)
                                                  :leader_id (or (:leader_id ancestor) (str parent))
                                                  :team_id (or (:team_id ancestor) (str turn))
                                                  :depth (inc (or (:depth ancestor) 0))
                                                  :task "Read existing evidence"
                                                  :iteration_budget 4
                                                  :spawn_key (str turn)
                                                  :spawn_fingerprint "fixture"
                                                  :checkpoint [{:role :user
                                                                :content "Full current context"}]}
                                                 opts)}))))

(deftest managed-lineage-and-context-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        other
        (str (h/store-session! db {:channel :api}))

        child
        (child! db leader {})

        metadata
        (agents/info db child)

        env
        {:db-info db :session-id child :turn-state-atom (atom {})}]

    (is (= leader (:parent_id metadata)))
    (is (= "subagent" (get (agents/context env) "role")))
    (is (= "leader" (get (agents/context {:db-info db :session-id other}) "role")))
    (is (agents/wake-allowed? db leader child))
    (is (agents/wake-allowed? db child leader))
    (is (agents/wake-allowed? db child child))
    (is (not (agents/wake-allowed? db other child)))
    (is (agents/wake-allowed? db leader other))
    (is (not (agents/wake-allowed? db child other)))
    (is (= [{:role :user :content "Full current context"} {:role :user :content "Delegated task"}]
           (:messages (agents/inherited-base env 2 [{:role :user :content "Delegated task"}] []))))
    (is (nil? (agents/inherited-base env 3 [] [])))
    (is (not= :running (:status (last (ps/db-list-session-turns db child)))))
    (is (not (some #(= child (str (:id %))) (ps/db-list-sessions db :all))))
    (let [grandchild (child! db child {})]
      (is (agents/controls? db leader grandchild))
      (is (agents/controls? db child grandchild))
      (is (not (agents/controls? db grandchild child))))))

(deftest durable-budget-cancellation-and-router-test
  (let [db
        (h/store)

        leader
        (h/store-session! db {:channel :api})

        child
        (child! db leader {:iteration_budget 1 :allowed_models [{:provider "p" :model "small"}]})

        env
        {:db-info db :session-id child}

        router
        {:providers [{:id :p :root "large" :models [{:name "small"} {:name "large"}]}
                     {:id :other :root "small" :models [{:name "small"}]}]}]

    (is (= [{:id :p :root "small" :models [{:name "small"}]}]
           (:providers (agents/restrict-router env router))))
    (is (= 2 (count (:providers router))) "The shared router was not mutated")
    (is (agents/claim-iteration! env))
    (is (agents/wake-allowed? db child leader) "The final paid iteration can return its result")
    (is (not (agents/claim-iteration! env)))
    (is (= "budget_limited" (:status (agents/info db child))))
    (is (not (agents/wake-allowed? db leader child)))
    (is (agents/wake-allowed? db child leader)
        "Exhaustion can be reported without rerunning the child")
    (agents/finish! db child "completed")
    (is (= "budget_limited" (:status (agents/info db child))))
    (smodel/set-model! db leader "p" "small")
    (is (ps/db-routing-locked? db leader))
    (smodel/set-model! db leader nil nil)
    (is (not (ps/db-routing-locked? db leader)))))

(deftest gateway-spawn-is-idempotent-and-owned-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        gid
        (str (:id (ps/db-create-project! db {:name "Agent team"})))

        router
        {:providers [{:id :p :root "small" :models [{:name "small"}]}]}

        checkpoint
        [{:role :system :content "Rules"} {:role :user :content "Current task and folded evidence"}]

        env
        {:db-info db
         :session-id leader
         :router router
         :turn-state-atom (atom {:agent-checkpoint checkpoint :council {:activation-id "parent"}})}

        launches
        (atom [])]

    (ps/db-set-session-project! db leader gid)
    (ps/db-store-session-turn!
      db
      {:parent-session-id leader :user-request "Parent task" :status :running})
    (with-redefs-fn {#'lp/db-info (constantly db)
                     #'loop-router/get-router (constantly router)
                     #'toggles/enabled? (constantly true)
                     #'council/runtime (fn [_]
                                         {leader {:activation-id "parent" :group-id gid}})
                     (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                         env)
                     (ns-resolve 'com.blockether.vis.internal.council.core 'runtime-waker)
                     (atom {:eligible? (constantly true)
                            :wake! (fn [_ sid _]
                                     (swap! launches conj sid))})}
      (fn []
        (let [opts
              {:task "Check the isolated test owner; report evidence" :key "tests"}

              child
              (gateway/agents-operation! leader :spawn opts)

              sid
              (:session_id child)]

          (is (= [sid] @launches))
          (is (= checkpoint (ps/db-agent-checkpoint db sid)))
          (is (= gid (str (:project-id (ps/db-get-session db sid)))))
          (is (= sid (:session_id (gateway/agents-operation! leader :spawn opts))))
          (is (= [sid] @launches))
          (is (= :idempotency-conflict
                 (try (gateway/agents-operation! leader :spawn (assoc opts :task "Different task"))
                      nil
                      (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
          (is (= "cancelled"
                 (:status (gateway/agents-operation! leader :cancel {:session_id sid}))))
          (is (not (agents/claim-iteration! {:db-info db :session-id sid})))
          (is (= "cancelled" (:status (agents/info db sid)))))))))

(deftest child-usage-excludes-inherited-work-test
  (let [db
        (h/store)

        leader
        (h/store-session! db {:channel :api})

        turn
        (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent work"})]

    (h/store-iteration! db {:session-turn-id turn :code "" :tokens {"input" 9000 "output" 100}})
    (let [child
          (child! db leader {})

          own
          (ps/db-store-session-turn! db {:parent-session-id child :user-request "Child work"})]

      (h/store-iteration! db {:session-turn-id own :code "" :tokens {"input" 200 "output" 10}})
      (is (= 9000 (:input-tokens (ps/db-session-usage-stats db leader))))
      (is (= 200 (:input-tokens (ps/db-session-usage-stats db child))))
      (h/fork-session! db child {})
      (let [after-fold (ps/db-store-session-turn! db
                                                  {:parent-session-id child
                                                   :user-request "Work after folding"})]
        (h/store-iteration!
          db
          {:session-turn-id after-fold :code "" :tokens {"input" 300 "output" 10}})
        (is (= 500 (:input-tokens (ps/db-session-usage-stats db child)))
            "A fold resets local turn positions but must not hide the child's own usage")))))

(deftest terminal-outcomes-report-to-the-persisted-leader-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        child
        (child! db leader {:iteration_budget 1})

        published
        (atom [])]

    (is (agents/claim-iteration! {:db-info db :session-id child}))
    (is (not (agents/claim-iteration! {:db-info db :session-id child})))
    (with-redefs [council/enabled?
                  (constantly true)

                  council/publish!
                  (fn [_ _ author opts]
                    (swap! published conj [author opts]))]

      (#'gateway/report-agent-outcome! db child "turn" {:content [{"text" "Verified result"}]})
      (is (= [leader] (:ping (second (first @published)))))
      (is (re-find #"budget_limited" (:content (second (first @published)))))
      (is (re-find #"Verified result" (:content (second (first @published)))))
      (is (= "agent-result:turn" (:idempotency_key (second (first @published)))))
      (ps/db-agent-update! db child {:status "cancelled"})
      (#'gateway/report-agent-outcome! db child "stopped" {})
      (is (= [] (:ping (second (last @published))))))))

(deftest stopping-a-parent-arms-its-backstop-before-stopping-children-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        child
        (child! db leader {})

        token
        (cancellation/cancellation-token)

        child-token
        (cancellation/cancellation-token)

        armed
        (atom [])

        registry
        (atom {leader {:current-turn "parent-turn"
                       :turns {"parent-turn" {:status "running" :cancel-token token}}}
               child {:current-turn "child-turn"
                      :turns {"child-turn" {:status "running" :cancel-token child-token}}}})]

    (with-redefs-fn {#'lp/db-info (constantly db)
                     #'gateway/registry registry
                     #'gateway/start-cancel-terminal-backstop!
                     (fn [sid & _]
                       (when (= sid child)
                         (is (cancellation/cancelled? token)
                             "The parent cannot run another iteration while a child stop blocks"))
                       (swap! armed conj sid))}
      (fn []
        (is (= {:status "cancelling"} (gateway/cancel-turn! leader "parent-turn")))
        (is (= [leader child] @armed))
        (is (cancellation/cancelled? token))
        (is (cancellation/cancelled? child-token))
        (is (= "cancelled" (:status (agents/info db child))))))))

(deftest inactive-and-incomplete-delegations-do-not-allocate-children-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        router
        {:providers [{:id :p :root "small" :models [{:name "small"}]}]}

        turn-state
        (atom {:council {:activation-id "active"}})

        runtime
        (atom {})

        env
        {:db-info db :session-id leader :router router :turn-state-atom turn-state}

        refusal
        (fn []
          (try (gateway/agents-operation! leader :spawn {:task "Check evidence"})
               (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

    (ps/db-store-session-turn! db
                               {:parent-session-id leader :user-request "Parent" :status :running})
    (with-redefs-fn {#'lp/db-info (constantly db)
                     #'loop-router/get-router (constantly router)
                     #'council/enabled? (constantly true)
                     #'council/runtime (fn [_]
                                         @runtime)
                     (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                         env)}
      (fn []
        (is (= :inactive-session (refusal)))
        (reset! runtime {leader {:activation-id "active"}})
        (is (= :no-checkpoint (refusal)))
        (swap! turn-state assoc :agent-checkpoint [{:role :user :content "Current context"}])
        (reset! runtime {leader {:activation-id "different"}})
        (is (= :inactive-session (refusal)))
        (is (empty? (ps/db-agent-list db leader)))))))

(deftest persisted-team-and-active-limits-test
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        turn
        (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

        fork
        (fn [n]
          (str (h/fork-session-at-turn! db
                                        leader
                                        {:through-turn-id turn
                                         :agent {:parent_id leader
                                                 :leader_id leader
                                                 :team_id (str turn)
                                                 :depth 1
                                                 :task "Check evidence"
                                                 :iteration_budget 1
                                                 :spawn_key (str n)
                                                 :spawn_fingerprint (str n)
                                                 :checkpoint [{:role :user
                                                               :content "Current context"}]}})))

        refusal
        (fn [n]
          (try (fork n) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

    (dotimes [n 8]
      (fork n))
    (is (= :agent-limit (refusal 8)))
    (is (= 8 (count (ps/db-agent-list db leader))))
    (let [[idle blocker] (map :session_id (ps/db-agent-list db leader))]
      (ps/db-agent-update! db idle {:status "completed"})
      (fork 8)
      (is (not (ps/db-agent-claim-iteration! db idle))
          "Resuming a completed child cannot bypass the active team limit")
      (is (= 0 (:iterations_used (agents/info db idle))))
      (is (= "completed" (:status (agents/info db idle))))
      (is (not (agents/claim-iteration! {:db-info db :session-id idle})))
      (is (= "failed" (:status (agents/info db idle)))
          "Temporary capacity refusal must not permanently exhaust the child budget")
      (is (= 0 (:iterations_used (agents/info db idle))))
      (ps/db-agent-update! db blocker {:status "completed"})
      (is (agents/claim-iteration! {:db-info db :session-id idle})
          "A capacity refusal is retryable after another child settles"))
    (doseq [child (ps/db-agent-list db leader)]
      (ps/db-agent-update! db (:session_id child) {:status "completed"}))
    (doseq [n (range 9 32)]
      (ps/db-agent-update! db (fork n) {:status "completed"}))
    (is (= :agent-limit (refusal 32)))
    (is (= 32 (count (ps/db-agent-list db leader))))))
