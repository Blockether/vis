(ns com.blockether.vis.internal.session.goals-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.goals :as goals]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store!)

(defn- environment [] {:db-info (h/store) :session-id (h/store-session! (h/store) {:channel :api})})

(defn- rejected? [f] (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(deftest explicit-goals-test
  (let [{:keys [db-info session-id] :as env} (environment)]
    (is (nil? (goals/check-goal env)))
    (is (nil? (goals/completion-error env)))
    (let [goal (goals/set-goal! db-info session-id "  Verify the SDK  " 100)]
      (is (document/valid-json? "gateway" "session_goal" goal))
      (is (= 100 (get goal "iteration_budget")))
      (is (= 0 (get goal "iterations_used")))
      (is (not (contains? goal "token_budget")))
      (is (= goal (goals/check-goal env)))
      (is (= "Verify the SDK" (get goal "objective")))
      (is (= "active" (get goal "status")))
      (is (string? (goals/completion-error env)))
      (is (false? (persistence/db-compare-session-goal! db-info session-id 0 goal)))
      (let [done (goals/update-goal env (get goal "id") 1 "complete" "SDK boundary tests pass.")]
        (is (= "complete" (get done "status")))
        (is (nil? (goals/completion-error env)))
        (is (= 2 (get done "revision")))
        (is (= 2 (get done "version")))))))

(deftest user-controls-and-stale-work-test
  (let [{:keys [db-info session-id] :as env}
        (environment)

        first-goal
        (goals/set-goal! db-info session-id "First" nil)]

    (is (= "paused" (get (goals/control! db-info session-id :pause) "status")))
    (is (rejected? #(goals/update-goal env (get first-goal "id") 1 "complete" "Stale evidence")))
    (is (= "active" (get (goals/control! db-info session-id :resume) "status")))
    (goals/account! env first-goal {:input-tokens 10 :output-tokens 5} 20)
    (is (= 0 (get (goals/check-goal env) "tokens_used")))
    (let [replacement (goals/set-goal! db-info session-id "Second" nil)]
      (is (not= (get first-goal "id") (get replacement "id")))
      (is (= 4 (get replacement "revision")))
      (is (rejected? #(goals/update-goal env (get first-goal "id") 1 "blocked" "Old blocker")))
      (is (= "cancelled" (get (goals/control! db-info session-id :cancel) "status")))
      (is (rejected? #(goals/control! db-info session-id :resume))))))

(deftest iteration-budget-test
  (let [{:keys [db-info session-id] :as env}
        (environment)

        goal
        (goals/set-goal! db-info session-id "Bounded work" 1)]

    (goals/account! env goal {:input-tokens 10 :output-tokens 7 :cache-read-tokens 8} 23)
    (is (= "active" (get (goals/check-goal env) "status")))
    (is (nil? (goals/halt-result env goal)))
    (is (some? (goals/request-halt-result env goal)))
    (let [limited (goals/check-goal env)]
      (is (= 1 (get limited "iterations_used")))
      (is (= 17 (get limited "tokens_used")))
      (is (= 23 (get limited "time_used_ms")))
      (is (= "budget_limited" (get limited "status")))
      (is (nil? (goals/completion-error env)))
      (is (rejected? #(goals/control! db-info session-id :resume)))
      (is (rejected? #(goals/update-goal env (get goal "id") 1 "complete" "Too late")))
      (is (= 1 (get limited "version"))))))

(deftest iteration-usage-survives-resume-and-reset-is-explicit-test
  (let [{:keys [db-info session-id] :as env}
        (environment)

        goal
        (goals/set-goal! db-info session-id "Bounded" 3)]

    (goals/account! env goal nil 0)
    (goals/control! db-info session-id :pause)
    (let [resumed (goals/control! db-info session-id :resume)]
      (is (= 1 (get resumed "iterations_used")))
      (is (= 3 (get resumed "iteration_budget")))
      (goals/account! env resumed nil 0)
      (is (= 2 (get (goals/check-goal env) "iterations_used")))
      (is (nil? (goals/request-halt-result env resumed)))
      (goals/account! env resumed nil 0)
      (is (some? (goals/request-halt-result env resumed))))
    (let [replacement (goals/set-goal! db-info session-id "New scope" nil)]
      (is (= 0 (get replacement "iterations_used")))
      (is (nil? (get replacement "iteration_budget")))
      (is (not (document/valid-json? "gateway"
                                     "session_goal"
                                     (assoc replacement "token_budget" 100)))))))

(deftest slash-parsing-test
  (let [{:keys [db-info session-id]}
        (environment)

        invoke
        #(goals/slash! {:db-info db-info :session/id session-id :command/raw %})]

    (is (= :ok (:slash/status (invoke "/goal --budget 40 -- Keep \"quotes\"
and newlines"))))
    (is (= "Keep \"quotes\"
and newlines" (get (goals/check-goal db-info session-id) "objective")))
    (is (= 40 (get (goals/check-goal db-info session-id) "iteration_budget")))
    (is (false? (get-in (invoke "/goal --pause") [:slash/data :goal-run?])))
    (is (true? (get-in (invoke "/goal --resume") [:slash/data :goal-run?])))
    (doseq [raw ["/goal" "/goal --budget 0 work" "/goal --budget 999999999999999999999 work"
                 "/goal --budget -1 work" "/goal --unknown"]]
      (is (= :error (:slash/status (invoke raw))) raw))))

(deftest model-cannot-create-or-expand-test
  (let [{:keys [db-info session-id] :as env} (environment)]
    (is (rejected? #(goals/update-goal env "missing" 1 "complete" "No goal")))
    (let [goal (goals/set-goal! db-info session-id "Original scope" nil)]
      (doseq [status ["active" "paused" "cancelled"]]
        (is (rejected? #(goals/update-goal env (get goal "id") 1 status "No"))))
      (is (rejected? #(goals/update-goal env (get goal "id") 1 "complete" "  ")))
      (is (= goal (goals/check-goal env))))))

(deftest mutation-events-test
  (let [{:keys [db-info session-id] :as env}
        (environment)

        seen
        (atom [])

        listener
        (goals/add-listener! #(swap! seen conj [%1 %2]))]

    (try (let [goal (goals/set-goal! db-info session-id "Notify" nil)]
           (goals/check-goal env)
           (is (= [[session-id goal]] @seen))
           (goals/control! db-info session-id :cancel)
           (is (= [1 2] (mapv #(get (second %) "revision") @seen))))
         (finally (goals/remove-listener! listener)))))

(deftest stopped-turn-cannot-pause-a-replacement-test
  (let [{:keys [db-info session-id] :as env}
        (environment)

        first-goal
        (goals/set-goal! db-info session-id "First" nil)]

    (goals/finish-turn! env first-goal :cancelled)
    (is (= "paused" (get (goals/check-goal env) "status")))
    (let [replacement (goals/set-goal! db-info session-id "Replacement" nil)]
      (goals/finish-turn! env first-goal :error)
      (is (= replacement (goals/check-goal env)))
      (is (= :cancelled (:status (goals/halt-result env first-goal)))))))
