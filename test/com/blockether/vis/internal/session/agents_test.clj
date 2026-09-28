(ns com.blockether.vis.internal.session.agents-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
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
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(defdescribe
  schema-owned-agent-bounds-test
  (it "schema owned agent bounds"
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

        (expect (= agents/default-iterations
                   (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"])
                   32))
        (expect
          (= agents/max-depth (get-in schema ["$defs" "agent" "properties" "depth" "maximum"]) 2))
        (expect (= agents/max-team-children (get schema "x-vis-max-team-children") 32))
        (expect (= agents/max-active-children (get schema "x-vis-max-active-children") 8))
        (expect (document/valid? "agents" agent))
        (expect (not (document/valid? "agents" (assoc agent "depth" 3))))
        (expect (not (document/valid? "agents" (assoc agent "iteration_budget" 201))))
        (expect (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8192 "x"))}))
        (expect
          (not (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8193 "x"))}))))))

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

(defdescribe
  managed-lineage-and-context-test
  (it
    "managed lineage and context"
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

      (expect (= leader (:parent_id metadata)))
      (expect (= "subagent" (get (agents/context env) "role")))
      (expect (= "leader" (get (agents/context {:db-info db :session-id other}) "role")))
      (expect (agents/wake-allowed? db leader child))
      (expect (agents/wake-allowed? db child leader))
      (expect (agents/wake-allowed? db child child))
      (expect (not (agents/wake-allowed? db other child)))
      (expect (agents/wake-allowed? db leader other))
      (expect (not (agents/wake-allowed? db child other)))
      (expect
        (= [{:role :user :content "Full current context"} {:role :user :content "Delegated task"}]
           (:messages (agents/inherited-base env 2 [{:role :user :content "Delegated task"}] []))))
      (expect (nil? (agents/inherited-base env 3 [] [])))
      (expect (not= :running (:status (last (ps/db-list-session-turns db child)))))
      (expect (not (some #(= child (str (:id %))) (ps/db-list-sessions db :all))))
      (let [grandchild (child! db child {})]
        (expect (agents/controls? db leader grandchild))
        (expect (agents/controls? db child grandchild))
        (expect (not (agents/controls? db grandchild child)))))))

(defdescribe
  durable-budget-cancellation-and-router-test
  (it
    "durable budget cancellation and router"
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

      (expect (= [{:id :p :root "small" :models [{:name "small"}]}]
                 (:providers (agents/restrict-router env router))))
      (expect (= 2 (count (:providers router))) "The shared router was not mutated")
      (expect (agents/claim-iteration! env))
      (expect (agents/wake-allowed? db child leader)
              "The final paid iteration can return its result")
      (expect (not (agents/claim-iteration! env)))
      (expect (= "budget_limited" (:status (agents/info db child))))
      (expect (not (agents/wake-allowed? db leader child)))
      (expect (agents/wake-allowed? db child leader)
              "Exhaustion can be reported without rerunning the child")
      (agents/finish! db child "completed")
      (expect (= "budget_limited" (:status (agents/info db child))))
      (smodel/set-model! db leader "p" "small")
      (expect (ps/db-routing-locked? db leader))
      (smodel/set-model! db leader nil nil)
      (expect (not (ps/db-routing-locked? db leader))))))

(defdescribe
  gateway-spawn-is-idempotent-and-owned-test
  (it
    "gateway spawn is idempotent and owned"
    (doseq [grouped? [false true]]
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            project-id (str (:id (ps/db-create-project! db {:name "Agent team"})))
            group-id (when grouped?
                       (str (:id (ps/db-create-session-group! db project-id {:name "Research"}))))
            gid (or group-id project-id)
            router {:providers [{:id :p :root "small" :models [{:name "small"}]}]}
            checkpoint [{:role :system :content "Rules"}
                        {:role :user :content "Current task and folded evidence"}]
            env {:db-info db
                 :session-id leader
                 :router router
                 :turn-state-atom (atom {:agent-checkpoint checkpoint
                                         :council {:activation-id "parent"}})}
            launches (atom [])]

        (ps/db-set-session-project! db leader project-id)
        ;; Managed children must join an explicit Council group before delegation.
        (when group-id (ps/db-set-session-group! db leader group-id))
        (ps/db-store-session-turn!
          db
          {:parent-session-id leader :user-request "Parent task" :status :running})
        (with-redefs-fn {#'lp/db-info (constantly db)
                         #'loop-router/get-router (constantly router)
                         #'toggles/enabled? (constantly true)
                         #'council/runtime (fn [_]
                                             {leader {:activation-id "parent" :group-id gid}})
                         (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env)
                         (constantly env)
                         (ns-resolve 'com.blockether.vis.internal.council.core 'runtime-waker)
                         (atom {:eligible? (constantly true)
                                :wake! (fn [_ sid _]
                                         (swap! launches conj sid))})}
          (fn []
            (let [opts {:task "Check the isolated test owner; report evidence" :key "tests"}
                  child (gateway/agents-operation! leader :spawn opts)
                  sid (:session_id child)]

              (expect (= [sid] @launches))
              (expect (= checkpoint (ps/db-agent-checkpoint db sid)))
              (expect (= project-id (str (:project-id (ps/db-get-session db sid)))))
              (expect (= group-id
                         (some-> (:group-id (ps/db-get-session db sid))
                                 str)))
              (expect (= gid (council/session-group db (ps/db-get-session db sid))))
              (expect (= sid (:session_id (gateway/agents-operation! leader :spawn opts))))
              (expect (= [sid] @launches))
              (expect (= :idempotency-conflict
                         (try (gateway/agents-operation! leader
                                                         :spawn
                                                         (assoc opts :task "Different task"))
                              nil
                              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
              (expect (= "cancelled"
                         (:status (gateway/agents-operation! leader :cancel {:session_id sid}))))
              (expect (not (agents/claim-iteration! {:db-info db :session-id sid})))
              (expect (= "cancelled" (:status (agents/info db sid)))))))))))

(defdescribe
  child-usage-excludes-inherited-work-test
  (it "child usage excludes inherited work"
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
          (expect (= 9000 (:input-tokens (ps/db-session-usage-stats db leader))))
          (expect (= 200 (:input-tokens (ps/db-session-usage-stats db child))))
          (h/fork-session! db child {})
          (let [after-fold (ps/db-store-session-turn! db
                                                      {:parent-session-id child
                                                       :user-request "Work after folding"})]
            (h/store-iteration!
              db
              {:session-turn-id after-fold :code "" :tokens {"input" 300 "output" 10}})
            (expect
              (= 500 (:input-tokens (ps/db-session-usage-stats db child)))
              "A fold resets local turn positions but must not hide the child's own usage"))))))

(defdescribe
  terminal-outcomes-report-to-the-persisted-leader-test
  (it "terminal outcomes report to the persisted leader"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {:iteration_budget 1})

            published
            (atom [])]

        (expect (agents/claim-iteration! {:db-info db :session-id child}))
        (expect (not (agents/claim-iteration! {:db-info db :session-id child})))
        (with-redefs [council/enabled?
                      (constantly true)

                      council/publish!
                      (fn [_ _ author opts]
                        (swap! published conj [author opts]))]

          (#'gateway/report-agent-outcome! db child "turn" {:content [{"text" "Verified result"}]})
          (expect (= [leader] (:ping (second (first @published)))))
          (expect (re-find #"budget_limited" (:content (second (first @published)))))
          (expect (re-find #"Verified result" (:content (second (first @published)))))
          (expect (= "agent-result:turn" (:idempotency_key (second (first @published)))))
          (ps/db-agent-update! db child {:status "cancelled"})
          (#'gateway/report-agent-outcome! db child "stopped" {})
          (expect (= [] (:ping (second (last @published)))))))))

(defdescribe
  stopping-a-parent-arms-its-backstop-before-stopping-children-test
  (it
    "stopping a parent arms its backstop before stopping children"
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
                           (expect
                             (cancellation/cancelled? token)
                             "The parent cannot run another iteration while a child stop blocks"))
                         (swap! armed conj sid))}
        (fn []
          (expect (= {:status "cancelling"} (gateway/cancel-turn! leader "parent-turn")))
          (expect (= [leader child] @armed))
          (expect (cancellation/cancelled? token))
          (expect (cancellation/cancelled? child-token))
          (expect (= "cancelled" (:status (agents/info db child)))))))))

(defdescribe
  inactive-and-incomplete-delegations-do-not-allocate-children-test
  (it
    "inactive and incomplete delegations do not allocate children"
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

      (ps/db-store-session-turn!
        db
        {:parent-session-id leader :user-request "Parent" :status :running})
      (with-redefs-fn {#'lp/db-info (constantly db)
                       #'loop-router/get-router (constantly router)
                       #'council/enabled? (constantly true)
                       #'council/runtime (fn [_]
                                           @runtime)
                       (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                           env)}
        (fn []
          (expect (= :inactive-session (refusal)))
          (reset! runtime {leader {:activation-id "active"}})
          (expect (= :no-checkpoint (refusal)))
          (swap! turn-state assoc :agent-checkpoint [{:role :user :content "Current context"}])
          (reset! runtime {leader {:activation-id "different"}})
          (expect (= :inactive-session (refusal)))
          (expect (empty? (ps/db-agent-list db leader))))))))

(defdescribe
  persisted-team-and-active-limits-test
  (it
    "persisted team and active limits"
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
      (expect (= :agent-limit (refusal 8)))
      (expect (= 8 (count (ps/db-agent-list db leader))))
      (let [[idle blocker] (map :session_id (ps/db-agent-list db leader))]
        (ps/db-agent-update! db idle {:status "completed"})
        (fork 8)
        (expect (not (ps/db-agent-claim-iteration! db idle))
                "Resuming a completed child cannot bypass the active team limit")
        (expect (= 0 (:iterations_used (agents/info db idle))))
        (expect (= "completed" (:status (agents/info db idle))))
        (expect (not (agents/claim-iteration! {:db-info db :session-id idle})))
        (expect (= "failed" (:status (agents/info db idle)))
                "Temporary capacity refusal must not permanently exhaust the child budget")
        (expect (= 0 (:iterations_used (agents/info db idle))))
        (ps/db-agent-update! db blocker {:status "completed"})
        (expect (agents/claim-iteration! {:db-info db :session-id idle})
                "A capacity refusal is retryable after another child settles"))
      (doseq [child (ps/db-agent-list db leader)]
        (ps/db-agent-update! db (:session_id child) {:status "completed"}))
      (doseq [n (range 9 32)]
        (ps/db-agent-update! db (fork n) {:status "completed"}))
      (expect (= :agent-limit (refusal 32)))
      (expect (= 32 (count (ps/db-agent-list db leader)))))))
