(ns com.blockether.vis.internal.session.agents-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe describe expect it]]))

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
             "iteration_budget" 32
             "iterations_used" 0}]

        (expect (= agents/default-iterations
                   (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"])
                   120))
        (expect (= agents/max-team-children (get schema "x-vis-max-team-children") 128))
        (expect (= agents/max-active-children (get schema "x-vis-max-active-children") 16))
        (expect (document/valid? "agents" agent))
        (expect (not (document/valid? "agents" (assoc agent "iteration_budget" 601))))
        (expect (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8192 "x"))}))
        (expect (document/valid-json? "agents"
                                      "spawn"
                                      {"task" "Edit one owner"
                                       "files" {"write" ["src/a/**"] "deny" [".env"]}}))
        (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "files" {}})))
        (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "files" {"exec" ["a"]}})))
        (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "files" {"write" []}})))
        (expect
          (not (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 8193 "x"))}))))))

(defn child!
  [db parent opts]
  (let [turn (ps/db-store-session-turn!
               db
               {:parent-session-id parent :user-request "Parent task" :status :running})]
    (str (h/fork-session-at-turn! db
                                  parent
                                  {:through-turn-id turn
                                   :agent (merge {:parent_id (str parent)
                                                  :leader_id (str parent)
                                                  :team_id (str turn)
                                                  :task "Read existing evidence"
                                                  :iteration_budget 4
                                                  :spawn_key (str turn)
                                                  :spawn_fingerprint "fixture"
                                                  :checkpoint [{:role :user
                                                                :content "Full current context"}]}
                                                 opts)}))))

;; The leader computed whole answers itself and spawned children only to confirm them. The rules
;; follow the Codex and Claude Code delegation rules and the Vis subagent evaluation.
(defdescribe
  delegation-decision-rules-test
  (it "tells the leader when to delegate, how many children to use and what to keep"
      (let [text (agents/prompt {})]
        (doseq [rule ["When in doubt, do the work yourself." "Do the critical-path step yourself"
                      "Do shared work yourself before you split" "usually two to four in total"
                      "give the child the code, not your conclusion" "Do not do their work again."]]
          (expect (str/includes? text rule) rule))
        (expect (not (str/includes? text "Verify answers before you use them"))
                "A general verify rule made the leader redo the delegated work"))))

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
      ;; #202: only a managed subagent wakes itself; an independent leader stays idle.
      (expect (not (agents/wake-allowed? db leader leader)))
      (expect (not (agents/wake-allowed? db other other)))
      (expect
        (= [{:role :user :content "Full current context"} {:role :user :content "Delegated task"}]
           (:messages (agents/inherited-base env 2 [{:role :user :content "Delegated task"}] []))))
      (expect (nil? (agents/inherited-base env 3 [] [])))
      (expect (not= :running (:status (last (ps/db-list-session-turns db child)))))
      (expect (not (some #(= child (str (:id %))) (ps/db-list-sessions db :all))))
      (expect (agents/controls? db leader child))
      (expect (agents/controls? db child child))
      (expect (not (agents/controls? db child leader))))))

;; Regression: a subagent sent its own session id as the OpenAI prompt cache key. Its first
;; request held the leader's whole prefix but missed the leader's warm cache (0% cache read).
(defdescribe team-shares-the-leader-cache-key-test
             (it "gives every subagent of a team the leader's id as its prompt cache key"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       other
                       (str (h/store-session! db {:channel :api}))

                       child
                       (child! db leader {})]

                   (expect (= leader (agents/leader-id db leader)))
                   (expect (= leader (agents/leader-id db child)))
                   (expect (= other (agents/leader-id db other))
                           "An independent session keeps its own key")
                   (expect (nil? (agents/leader-id db nil))))))

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
      (expect
        (=
          "Subagent stopped: it used its full iteration budget of 1 and its handoff iteration. Its parent can add iterations with council.extend and continue it with council.resume."
          (agents/stop-reason env)))
      (expect (not (agents/wake-allowed? db leader child)))
      (expect (agents/wake-allowed? db child leader)
              "Exhaustion can be reported without rerunning the child")
      (agents/finish! db child "completed")
      (expect (= "budget_limited" (:status (agents/info db child))))
      (smodel/set-model! db leader "p" "small")
      (expect (ps/db-routing-locked? db leader))
      (smodel/set-model! db leader nil nil)
      (expect (not (ps/db-routing-locked? db leader))))))

(defn- with-fake-runtime
  "Call `(f entries)` with a runtime whose Council lives in memory. A reply to an entry is
   visible through `ps/db-council-get`, as in the gateway. `entries` is the atom of the
   published entries. Restores the gateway runtime, and registers the Council ops for
   direct calls."
  [db leader live f]
  (let [entries
        (atom {})

        saved
        @@#'agents/runtime

        registered?
        (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions))]

    (when-not registered? (foundation/register!))
    (agents/install-runtime!
      {:live? (fn [sid]
                (contains? @live (str sid)))
       :list (fn [_ _]
               (ps/db-agent-list db leader))
       :publish (fn [env opts]
                  (let [id (inc (count @entries))]
                    (swap! entries assoc id (assoc opts :author (str (:session-id env))))
                    (when-let [to (:reply_to opts)]
                      (swap! entries update-in [to :replies] (fnil conj []) {:reply_entry_id id}))
                    {:entry_id id :replies [{:state "delivered"}]}))})
    (try (f entries)
         (finally (agents/install-runtime! saved)
                  (when-not registered? (extension/deregister-extension! "foundation-core"))))))

(defn- outcome [f] (try (:result (f)) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

(defn- asking-record
  "Poll the record of `child` until it asks, for at most 5 s."
  [env child]
  (loop [n 0]
    (let [r (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))]
      (if (or (= "asking" (get r "status")) (> n 50)) r (do (Thread/sleep 100) (recur (inc n)))))))

;; Issue #388: a child had 64 iterations, no warning and a hard stop without a handoff. A
;; parent could not add iterations, so the only option was a new child without the context.
(defdescribe
  subagent-budget-test
  (it
    "warns the child near the end of its budget"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          child
          (child! db leader {:iteration_budget 40})

          env
          {:db-info db :session-id child}]

      (expect (nil? (get (agents/context env) "budget_notice")))
      (dotimes [_ 32]
        (agents/claim-iteration! env))
      (expect
        (=
          "8 of 40 iterations left. Finish the task, or call council.ask_budget(iterations, reason) for more. Before the budget ends, report the done work, the changed files and the next step."
          (get (agents/context env) "budget_notice")))
      (dotimes [_ 8]
        (agents/claim-iteration! env))
      (expect (re-find #"^Your iteration budget is used\. Your next response is your handoff\."
                       (get (agents/context env) "budget_notice")))))
  (it "gives one handoff iteration after the budget and then stops"
      (let [db
            (h/store)

            leader
            (h/store-session! db {:channel :api})

            child
            (child! db leader {:iteration_budget 1})

            env
            {:db-info db :session-id child :turn-state-atom (atom {})}]

        (expect (agents/claim-iteration! env))
        (expect (agents/claim-iteration! env) "The handoff iteration runs without a claim")
        (expect (= 1 (:iterations_used (agents/info db child))))
        (expect (not= "budget_limited" (:status (agents/info db child))))
        (expect (not (agents/claim-iteration! env)) "Only one handoff iteration")
        (expect (= "budget_limited" (:status (agents/info db child))))
        (expect (re-find #"council\.extend" (agents/stop-reason env)))
        (expect (re-find #"Last report:\nParser fixed\.$"
                         (agents/refusal-answer env "Parser fixed.")))
        (expect (re-find #"no report\. Read its work with read_session\(\""
                         (agents/refusal-answer env nil))
                "A handoff without prose still tells the parent where the work is")))
  (it "marks a child budget_limited only when its turn ends in the handoff iteration"
      (let [db
            (h/store)

            leader
            (h/store-session! db {:channel :api})

            done
            (child! db leader {:iteration_budget 1})

            cut
            (child! db leader {:iteration_budget 1})

            cut-env
            {:db-info db :session-id cut :turn-state-atom (atom {})}]

        (agents/claim-iteration! {:db-info db :session-id done :turn-state-atom (atom {})})
        (agents/finish! db done "completed")
        (expect (= "completed" (:status (agents/info db done)))
                "An answer on the last claimed iteration is a finished task")
        (agents/claim-iteration! cut-env)
        (agents/claim-iteration! cut-env)
        (agents/finish! db cut "completed")
        (expect (= "budget_limited" (:status (agents/info db cut))))))
  (it
    "extends a budget within the limits and keeps the child budget_limited until it continues"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 1})

          env
          {:db-info db :session-id leader}]

      (with-fake-runtime
        db
        leader
        (atom #{})
        (fn [_]
          (let [child-env {:db-info db :session-id child :turn-state-atom (atom {})}]
            (agents/claim-iteration! child-env)
            (agents/claim-iteration! child-env))
          (agents/finish! db child "completed")
          (expect (not (agents/wake-allowed? db leader child)))
          (expect (= {"status" "budget_limited" "iteration_budget" 6 "iterations_used" 1}
                     (select-keys (first (outcome #(agents/extend-agent env child 5)))
                                  ["status" "iteration_budget" "iterations_used"])))
          (expect (agents/wake-allowed? db leader child)
                  "A ping continues an extended child, like council.resume")
          ;; council.resume runs the child again.
          (ps/db-agent-update! db child {:status "running"})
          (expect (= :budget-exceeded (outcome #(agents/extend-agent env child 595))))
          (expect (= :invalid-request (outcome #(agents/extend-agent env child 0))))
          (expect (= :not-owner
                     (outcome #(agents/extend-agent {:db-info db :session-id other} child 1))))
          (expect (= :not-owner
                     (outcome #(agents/extend-agent {:db-info db :session-id child} child 1))))
          (ps/db-agent-update! db child {:status "cancelled"})
          (expect (= :invalid-request (outcome #(agents/extend-agent env child 1))))))))
  (it
    "lets a child ask for iterations and the parent grant or deny them"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 4})

          env
          {:db-info db :session-id leader}

          child-env
          {:db-info db :session-id child}]

      (with-fake-runtime
        db
        leader
        (atom #{leader})
        (fn [entries]
          (with-redefs [ps/db-council-get (fn [_ id]
                                            (get @entries id))]
            (let [asked (future (outcome #(agents/ask-budget child-env
                                                             10
                                                             "The suite has 40 files"
                                                             {:timeout_secs 10})))
                  r (asking-record env child)]

              (expect (= {"iterations" 10 "reason" "The suite has 40 files"}
                         (get r "budget_request")))
              (expect (= "Budget request: 10 more iterations. Reason: The suite has 40 files"
                         (get r "question")))
              (outcome #(agents/extend-agent env child 10))
              (expect (= {"granted" 10
                          "iteration_budget" 14
                          "reply" "Granted 10 more iterations. New budget: 14."}
                         (deref asked 5000 ::timeout))))
            (let [asked (future
                          (outcome
                            #(agents/ask-budget child-env 50 "More tests" {:timeout_secs 10})))]
              (asking-record env child)
              (expect (nil? (get (first (outcome
                                          #(agents/deny-agent env child "Stop at the parser.")))
                                 "budget_request"))
                      "The answered request leaves the record")
              (let [denied (deref asked 5000 ::timeout)]
                (expect (= 0 (get denied "granted")))
                (expect (re-find #"^Denied: Stop at the parser\." (get denied "reply")))))
            (expect (= :invalid-request (outcome #(agents/deny-agent env child "No request")))))
          (expect (= :invalid-request (outcome #(agents/ask-budget child-env 601 "Too many"))))
          (expect (= :invalid-request (outcome #(agents/ask-budget env 5 "Not a child"))))))))
  ;; Review of #388: a budget request in the handoff iteration used that iteration. After a
  ;; refusal, the child stopped without a report.
  (it "gives one more handoff iteration once after a refused request in the handoff"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {:iteration_budget 1})

            child-env
            {:db-info db :session-id child :turn-state-atom (atom {})}]

        (with-fake-runtime
          db
          leader
          (atom #{})
          (fn [_]
            (expect (agents/claim-iteration! child-env))
            (expect (agents/claim-iteration! child-env) "The handoff iteration")
            (expect (= 0 (get (outcome #(agents/ask-budget child-env 10 "More files")) "granted")))
            (expect (agents/claim-iteration! child-env) "A refusal renews the handoff")
            (outcome #(agents/ask-budget child-env 10 "More files"))
            (expect (not (agents/claim-iteration! child-env)) "Only one renewal in a turn")
            (expect (= "budget_limited" (:status (agents/info db child))))))))
  (it
    "resumes a finished child with a follow-up task and never returns its old answer"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 4})

          live
          (atom #{})

          env
          {:db-info db :session-id leader}

          turns
          (atom [{:id "old" :content [{"type" "prose" "markdown" "Old answer"}]}])]

      (with-fake-runtime
        db
        leader
        live
        (fn [entries]
          (with-redefs [ps/db-list-session-turns (fn [_ sid]
                                                   (if (= child (str sid)) @turns []))]
            (agents/finish! db child "completed")
            (expect (= "Old answer"
                       (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                            "answer")))
            (swap! live conj child)
            (expect (= :invalid-request (outcome #(agents/resume-agent env child "Next")))
                    "A running child needs council.wait")
            (swap! live disj child)
            (expect (= :not-owner
                       (outcome
                         #(agents/resume-agent {:db-info db :session-id child} child "Next"))))
            (let [records
                  (outcome
                    #(agents/resume-agent env child "Fix the parser test too" {:timeout_secs 1}))]
              (expect (= "queued" (get (first records) "status")))
              (expect (= 1 (:resumed_turns (agents/info db child)))
                      "The stale-answer guard is stored with the child, so a restart keeps it")
              (expect (= :invalid-request (outcome #(agents/resume-agent env child "Again")))
                      "A queued follow-up task blocks a second resume")
              (expect (= {:kind "coordination"
                          :title "Follow-up task"
                          :content "Fix the parser test too"
                          :ping [child]
                          :reply_required true
                          :author leader}
                         (get @entries (count @entries)))))
            ;; The gateway stores the status before the turn, so the old turn is still the last.
            (agents/finish! db child "completed")
            (expect (nil? (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                               "answer")))
            (swap! turns conj {:id "new" :content [{"type" "prose" "markdown" "New answer"}]})
            (expect (= "New answer"
                       (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                            "answer"))))
          (agents/claim-iteration! {:db-info db :session-id child})
          (dotimes [_ 3]
            (agents/claim-iteration! {:db-info db :session-id child}))
          (agents/finish! db child "completed")
          (expect (= :budget-exceeded (outcome #(agents/resume-agent env child "More")))
                  "Extend first")
          (ps/db-agent-update! db child {:status "budget_limited"})
          (outcome #(agents/extend-agent env child 2))
          (expect (= "budget_limited" (:status (agents/info db child))))
          (expect (= "queued"
                     (get (first (outcome
                                   #(agents/resume-agent env child "More" {:timeout_secs 1})))
                          "status"))
                  "An extended budget_limited child resumes")
          (ps/db-agent-update! db child {:status "cancelled"})
          (expect (= :invalid-request (outcome #(agents/resume-agent env child "More"))))))))
  ;; A Council ping and council.resume are one follow-up step, so both pass the active cap.
  (it
    "queues a pinged child like council.resume, past the active cap, and undoes it on demand"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})]

      (with-redefs [ps/db-list-session-turns (fn [_ sid]
                                               (if (= child (str sid)) [{:id "t1"}] []))]
        (agents/finish! db child "completed")
        (dotimes [_ agents/max-active-children]
          (child! db leader {}))
        (let [restore! (agents/queue-turn! db child)]
          (expect (fn? restore!))
          (expect (= {:status "queued" :resumed_turns 1}
                     (select-keys (agents/info db child) [:status :resumed_turns]))
                  "The stale-answer guard also holds for a ping")
          (expect (some? (:resumed_at (agents/info db child))))
          (expect (fn? (agents/queue-turn! db child)) "A queued child stays as is")
          (expect (= 1 (:resumed_turns (agents/info db child))))
          (restore!)
          (expect (= {:status "completed" :resumed_turns 0}
                     (select-keys (agents/info db child) [:status :resumed_turns]))))
        (dotimes [_ (:iteration_budget (agents/info db child))]
          (ps/db-agent-claim-iteration! db child))
        (ps/db-agent-update! db child {:status "budget_limited"})
        (expect (not (agents/wake-allowed? db leader child)) "An exhausted child stays idle")
        (expect (nil? (agents/queue-turn! db child)) "A child without iterations stays idle")
        (expect (some? (ps/db-agent-extend-budget! db child 2)))
        (expect (agents/wake-allowed? db leader child)
                "A ping continues an extended budget_limited child, like council.resume")
        (expect (fn? (agents/queue-turn! db child)))
        (expect (= "queued" (:status (agents/info db child))))
        (ps/db-agent-update! db child {:status "cancelled"})
        (expect (not (agents/wake-allowed? db leader child)))
        (expect (nil? (agents/queue-turn! db child))))))
  (it "continues a resume past the active cap"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})

            env
            {:db-info db :session-id leader}]

        (with-fake-runtime
          db
          leader
          (atom #{})
          (fn [_]
            (agents/finish! db child "completed")
            (dotimes [_ agents/max-active-children]
              (child! db leader {}))
            (expect (= "queued"
                       (get (first (outcome
                                     #(agents/resume-agent env child "More" {:timeout_secs 1})))
                            "status")))
            (expect (= "queued" (:status (agents/info db child)))))))))

;; Regression: Subagents was on only in the leader's session settings. A child starts without
;; those overrides, so its turn snapshot read the default and refused its first iteration.
(defdescribe
  leader-session-setting-governs-its-team-test
  (it
    "admits and stops children with the leader's session setting"
    (with-redefs [config/load-global-yaml-config-raw
                  (constantly {})

                  config/load-global-config-raw
                  (constantly {})

                  config/load-project-tiers-raw
                  (constantly {})

                  config/load-project-config-raw
                  (constantly {})]

      (toggles/set-value! "subagents" false)
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})

            env
            {:db-info db :session-id child}

            ;; A turn binds the settings of its own session, as `loop.turn/turn!` does.
            in-turn
            (fn [sid f]
              (binding [toggles/*overrides* (scoped/values db sid)]
                (f)))]

        (ps/db-set-scoped-setting! db "session" leader "subagents" true)
        (expect (false? (get (scoped/values db child) "subagents"))
                "A child starts without the session overrides of its leader")
        (expect (nil? (agents/require-enabled! db leader))
                "A gateway route outside a turn reads the session setting")
        (expect (in-turn child #(agents/claim-iteration! env)))
        (expect (in-turn child #(agents/wake-allowed? db child leader))
                "The child can return its outcome to an idle leader")
        (expect (in-turn leader #(agents/wake-allowed? db leader child)))
        (expect (in-turn child
                         #(and (nil? (agents/require-enabled! db child))
                               (every? (fn [entry]
                                         ((:ext.symbol/active-fn entry) env))
                                       agents/symbols)
                               (agents/prompt env)))
                "The child keeps its Council tools, its prompt and its spawn permission")
        (expect (str/includes? (in-turn child #(agents/prompt env))
                               (str "Only when it is ambiguous or misses a fact that you need, "
                                    "read your parent's whole session: "
                                    "await read_session(session[\"agent\"][\"parent_id\"])"))
                "A child reads its parent's whole session only on ambiguity")
        (ps/db-set-scoped-setting! db "session" leader "subagents" false)
        (expect (not (in-turn child #(agents/claim-iteration! env)))
                "Off in the leader's session stops further child iterations")
        (expect
          (=
            "Subagent stopped: Subagents are disabled for this team. Enable Subagents in Experimental settings."
            (in-turn child #(agents/stop-reason env))))
        (expect (not (in-turn leader #(agents/wake-allowed? db leader child))))))))

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
            router {:providers [{:id :p :root "small" :models [{:name "small"} {:name "large"}]}]}
            checkpoint [{:role :system :content "Rules"} {:role :user :content "Parent request"}
                        {:role :assistant
                         :content [{:type "text" :text "Note"}
                                   {:type "tool_use" :id "t1" :name "python_execution" :input {}}]}
                        {:role :user
                         :content [{:type "tool_result" :tool_use_id "t1" :content "Tool output"}]}
                        {:role :assistant :content "Answer"}
                        {:role :user :content "Current task and folded evidence"}]
            handoff [{:role :system :content "Rules"} {:role :user :content "Parent request"}
                     {:role :assistant :content "Note\n\nAnswer"}
                     {:role :user :content "Current task and folded evidence"}
                     (peek (agents/handoff [{:role :user :content "Any"}]))]
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
              (expect (= handoff (ps/db-agent-checkpoint db sid))
                      "A child gets the parent's conversation without its tool work by default")
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
              ;; A child on any model gets the handoff, and the spawn takes no context option.
              (let [other {:task "Review on another model" :provider "p" :model "large"}
                    task-child (:session_id (gateway/agents-operation! leader :spawn other))]

                (expect (= handoff (ps/db-agent-checkpoint db task-child)))
                (expect (= :invalid-request
                           (try
                             (gateway/agents-operation! leader :spawn (assoc other :context "full"))
                             nil
                             (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                ;; A file grant resolves against the parent's root and stays on the record.
                (let [grant {:task "Edit one owner" :files {:write ["src/owner/**"] :deny [".env"]}}
                      files (:files (ps/db-agent-info
                                      db
                                      (:session_id
                                        (gateway/agents-operation! leader :spawn grant))))]

                  (expect (str/ends-with? (first (:write files)) "/src/owner/**"))
                  (expect (str/starts-with? (first (:write files)) "/"))
                  (expect (str/ends-with? (first (:deny files)) "/.env"))
                  (expect (nil? (:read_only files))))
                ;; A grant never widens the parent's policy: the spawn names the problem.
                (with-redefs [security-policy/outside-write-rules (fn [_ rules _]
                                                                    (vec rules))]
                  (expect (str/includes?
                            (try (gateway/agents-operation! leader
                                                            :spawn
                                                            {:task "Write outside"
                                                             :files {:write ["/elsewhere"]}})
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (ex-message e)))
                            "files.write cannot grant /elsewhere: the parent cannot write there")))
                ;; Only a leader delegates: a child asks its parent for more subagents.
                (expect (= :not-allowed
                           (try (gateway/agents-operation! task-child :spawn {:task "Nested"})
                                nil
                                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                ;; The task message names the parent session whose tool work the child cannot see.
                (let [delegated (->> (ps/db-council-page db gid nil false 0 100)
                                     (filter #(= "Delegated task" (:title %)))
                                     (map :content))
                      hint (str "When the task is ambiguous or needs a fact from that tool output, "
                                "read it: s = await read_session(\""
                                leader
                                "\")")]

                  (expect (= 3 (count (filter #(str/includes? % hint) delegated))))))
              (expect (= "cancelled"
                         (:status (gateway/agents-operation! leader :cancel {:session_id sid}))))
              (expect (not (agents/claim-iteration! {:db-info db :session-id sid})))
              (expect (= "cancelled" (:status (agents/info db sid))))
              (expect (= "Subagent stopped: it is cancelled."
                         (agents/stop-reason {:db-info db :session-id sid}))))))))))

(defdescribe
  handoff-test
  (describe
    "the conversation that a task-context child inherits"
    (it "keeps system messages, requests, gists, notes and answers, and drops tool work"
        (expect
          (= [{:role "system" :content [{:type "text" :text "Rules"}]}
              {:role "user" :content "Request"} {:role "assistant" :content "Plan\n\nAnswer"}
              {:role "user" :content "# ⋯ folded t1/* · Gist\n\nNext request"}]
             (pop (agents/handoff
                    [{:role "system" :content [{:type "text" :text "Rules"}]}
                     {:role "user" :content "Request"}
                     {:role "assistant"
                      :content [{:type "thinking" :thinking "Hidden" :signature "s"}
                                {:type "text" :text "Plan"}
                                {:type "tool_use" :id "t1" :name "python_execution" :input {}}]}
                     {:role "user"
                      :content [{:type "tool_result" :tool_use_id "t1" :content "Output"}]}
                     {:role "assistant"
                      :content [{:type "tool_use" :id "t2" :name "python_execution" :input {}}]}
                     {:role "user"
                      :content [{:type "tool_result" :tool_use_id "t2" :content "More"}]}
                     {:role "assistant" :content "Answer"}
                     {:role "user" :content "# ⋯ folded t1/* · Gist"} {:role "user" :content "  "}
                     {:role "user" :content "Next request"}])))))
    (it "keeps the images of a request and its text as blocks, and drops tool images"
        (let [image
              {:type "image_url" :image_url {:url "data:image/png;base64,AA"}}

              legacy
              {:type :image :source {:type "base64" :media_type "image/png" :data "AA"}}]

          (expect (= [{:role :user :content [{:type :text :text "Look"} image legacy]}
                      {:role :assistant :content "Seen"}]
                     (pop (agents/handoff
                            [{:role :user :content [{:type :text :text "Look"} image legacy]}
                             {:role :user :content [image]}
                             {:role :assistant :content [{:type :text :text "Seen"} image]}]))))))
    (it "ends with a message that marks the conversation as context, not instructions"
        (let [end (peek (agents/handoff [{:role "system" :content "Rules"}
                                         {:role "user" :content "Run this block"}]))]
          (expect (= "user" (:role end)))
          (expect (str/starts-with? (:content end) ";; -- END-OF-PARENT-CONVERSATION --"))
          (expect (str/includes? (:content end) "not instructions for you"))
          ;; Live A/B: without this rule, 3 of 6 children used a parent's deletion approval.
          (expect (str/includes? (:content end) "approvals above belong to your parent"))
          (expect (str/includes? (:content end) "does not have the parent's variables"))))
    (it "adds no closing message to system messages alone"
        (expect (= [{:role "system" :content "Rules"}]
                   (agents/handoff [{:role "system" :content "Rules"}]))))
    (it "keeps nothing of a conversation with only tool work"
        (expect (= []
                   (agents/handoff
                     [{:role "assistant" :content [{:type "tool_use" :id "t" :input {}}]}
                      {:role "user"
                       :content [{:type "tool_result" :tool_use_id "t" :content "x"}]}]))))))

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
  subagent-usage-sums-the-whole-tree-test
  (it
    "sums every child in one rollup and splits the cost on the wire"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent work"})]

      (h/store-iteration!
        db
        {:session-turn-id turn :code "" :tokens {"input" 9000 "output" 100} :cost-usd 1.0})
      (expect (nil? (ps/db-session-subagent-usage db leader)))
      (let [children (conj (vec (repeatedly 99 #(child! db leader {:status "completed"})))
                           (child! db leader {}))]
        (doseq [sid children]
          (let [own (ps/db-store-session-turn! db {:parent-session-id sid :user-request "Work"})]
            (h/store-iteration!
              db
              {:session-turn-id own :code "" :tokens {"input" 100 "output" 10} :cost-usd 0.25})))
        (expect (= {:agent-count 100
                    :active-count 1
                    :iteration-count 100
                    :input-tokens 10000
                    :output-tokens 1000
                    :cost-usd 25.0}
                   (ps/db-session-subagent-usage db leader))
                "Inherited parent work is not counted again")
        (expect (nil? (ps/db-session-subagent-usage db (first children)))
                "A child has no subagents")
        (with-redefs [lp/db-info (constantly db)]
          (let [usage (gateway/session-usage-info leader)]
            (expect (= 1.0 (get usage "cost_usd")))
            (expect (= 26.0 (get usage "total_cost_usd")))
            (expect (= 100 (get-in usage ["subagents" "agent_count"])))
            (expect (= 25.0 (get-in usage ["subagents" "cost_usd"]))))
          (expect (not (contains? (gateway/session-usage-info (first children)) "subagents"))))))))

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

;; Regression: a gateway that died mid-team left its children queued or running forever,
;; and a resumed leader waited for children that no process ran any more.
(defdescribe
  orphaned-children-reconcile-test
  (it
    "fails queued and running children that no live process runs"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          dead
          (child! db leader {})

          queued
          (child! db leader {})

          done
          (child! db leader {})

          live
          (child! db leader {})

          other-leader
          (str (h/store-session! db {:channel :api}))

          spared
          (child! db other-leader {})]

      (ps/db-agent-update! db dead {:status "running"})
      (ps/db-agent-update! db done {:status "completed"})
      (ps/db-agent-update! db live {:status "running"})
      (ps/db-agent-update! db spared {:status "running"})
      (expect (= 2 (agents/reconcile-orphans! db [live other-leader])))
      (expect (= "failed" (:status (agents/info db dead))))
      (expect (= "failed" (:status (agents/info db queued))))
      (expect (= "completed" (:status (agents/info db done))))
      (expect (= "running" (:status (agents/info db live))) "A child with a live turn stays")
      (expect (= "running" (:status (agents/info db spared))) "A live leader still owns its team")
      (expect (= 0 (agents/reconcile-orphans! db [live other-leader]))
              "A second sweep finds nothing"))))

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
                                                   :task "Check evidence"
                                                   :iteration_budget 1
                                                   :spawn_key (str n)
                                                   :spawn_fingerprint (str n)
                                                   :checkpoint [{:role :user
                                                                 :content "Current context"}]}})))

          refusal
          (fn [n]
            (try (fork n) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

      ;; The store uses the contract limits, not its own copies.
      (dotimes [n agents/max-active-children]
        (fork n))
      (expect (= :agent-limit (refusal agents/max-active-children)))
      (expect (= agents/max-active-children (count (ps/db-agent-list db leader))))
      (let [[idle] (map :session_id (ps/db-agent-list db leader))]
        (ps/db-agent-update! db idle {:status "completed"})
        (fork agents/max-active-children)
        (expect (agents/claim-iteration! {:db-info db :session-id idle})
                "A woken child can pass the active cap; only a new spawn counts against it")
        (expect (= {:status "running" :iterations_used 1}
                   (select-keys (agents/info db idle) [:status :iterations_used]))))
      (doseq [child (ps/db-agent-list db leader)]
        (ps/db-agent-update! db (:session_id child) {:status "completed"}))
      (doseq [n (range (inc agents/max-active-children) agents/max-team-children)]
        (ps/db-agent-update! db (fork n) {:status "completed"}))
      (expect (= :agent-limit (refusal agents/max-team-children)))
      (expect (= agents/max-team-children (count (ps/db-agent-list db leader)))))))
