(ns com.blockether.vis.internal.session.goals-boundary-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [lazytest.core :refer [around-each set-ns-context!]]
            [com.blockether.vis.internal.session.goals :as goals]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

;; Own slash registration even after another suite temporarily loads and removes foundation.
(set-ns-context! [(around-each [f]
                               (let [registered? (some #(= "foundation-core" (:ext/name %))
                                                       (extension/registered-extensions))]
                                 (when-not registered? (foundation/register!))
                                 (try (f)
                                      (finally (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

(defn- environment
  []
  (lp/create-environment
    (svar/make-router
      [{:id :fixture :api-key "test" :base-url "http://127.0.0.1:1/v1" :models [{:name "model"}]}])
    {:db :memory}))

(deftest slash-context-python-and-continuation-test
  (let [env
        (environment)

        requests
        (atom 0)

        snapshots
        (atom [])]

    (try
      (with-redefs
        [svar/ask-code!
         (fn [_ opts]
           (is (str/includes? (str (:messages opts)) "update_goal"))
           (is (str/includes? (str (:messages opts)) "iteration_budget"))
           (swap! snapshots conj (ctx-loop/session-snapshot env))
           (case (swap! requests inc)
             1
             {:stop-reason :end
              :content "Premature answer"
              :api-usage {:input-tokens 5 :output-tokens 2}}

             2
             {:stop-reason :tool-calls
              :api-usage {:input-tokens 6 :output-tokens 3}
              :tool-calls
              [{:id "goal-update"
                :name "python_execution"
                :input
                {:code
                 "g = session['goal']\nprint(update_goal(g['id'], g['version'], 'complete', 'All requested checks passed.'))"}}]}

             3
             {:stop-reason :end
              :content "Verified and complete."
              :api-usage {:input-tokens 8 :output-tokens 2}}

             (throw (ex-info "Unexpected continuation" {}))))]
        (let [result (lp/run-turn! env "/goal Verify goal integration" {})]
          (is (= "Verified and complete." (get-in result [:answer :answer])))
          (is (= 3 @requests))
          (is (= "complete" (get (goals/check-goal env) "status")))
          (is (= 26 (get (goals/check-goal env) "tokens_used")))
          (is (= 3 (get (goals/check-goal env) "iterations_used")))
          (is (str/includes? (str @snapshots) "Verify goal integration"))))
      (finally (lp/dispose-environment! env)))))

(deftest iteration-budget-allows-last-tools-but-no-next-request-test
  ;; The shared slash boundary must enforce budgets after iOS substitutes a dash.
  (doseq [dash ["--" "—" "–"]]
    (let [env (environment)
          requests (atom 0)
          executions (atom [])
          execute @#'lp/execute-code]

      (try (with-redefs [lp/execute-code (fn [& args]
                                           (swap! executions conj (second args))
                                           (apply execute args))
                         svar/ask-code!
                         (fn [_ _]
                           (when (> (swap! requests inc) 1)
                             (throw (AssertionError. "Exceeded trailing iteration budget")))
                           {:stop-reason :tool-calls
                            :api-usage {:input-tokens 100000 :output-tokens 5}
                            :tool-calls [{:id "last-tools"
                                          :name "python_execution"
                                          :input {:code "print('last iteration ran')"}}
                                         {:id "more-tools"
                                          :name "python_execution"
                                          :input {:code
                                                  "print('second tool in same iteration')"}}]})]

             (let [result (lp/run-turn! env (str "/goal Bounded task " dash "budget 1") {})]
               (is (= :success (:status result)))
               (is (= 1 @requests))
               (is (= 2 (count @executions)))
               (is (= 1 (get (goals/check-goal env) "iterations_used")))
               (is (= "budget_limited" (get (goals/check-goal env) "status")))
               (is (str/includes? (str result) "last iteration ran"))
               (is (str/includes? (str result) "second tool in same iteration"))
               (is (not (str/includes? (str result) "Goal token budget")))
               (is (str/includes? (str (:answer result)) "iteration budget reached"))))
           (finally (lp/dispose-environment! env))))))

(deftest last-iteration-can-resolve-goal-test
  (doseq [status ["complete" "blocked"]]
    (let [env (environment)
          requests (atom 0)]

      (try (with-redefs [svar/ask-code!
                         (fn [_ _]
                           (when (> (swap! requests inc) 1)
                             (throw (AssertionError. "Exceeded iteration budget")))
                           {:stop-reason :tool-calls
                            :tool-calls
                            [{:id "resolve"
                              :name "python_execution"
                              :input
                              {:code
                               (str
                                 "g = session['goal']\nprint(update_goal(g['id'], g['version'], '"
                                 status
                                 "', 'Verified result or external blocker.'))")}}]})]
             (let [result (lp/run-turn! env "/goal --budget 1 Resolve on last iteration" {})]
               (is (= :success (:status result)))
               (is (= 1 @requests))
               (is (= status (get (goals/check-goal env) "status")))
               (is (= 1 (get (goals/check-goal env) "iterations_used")))
               (is (str/includes? (str (:answer result)) "Verified result or external blocker."))))
           (finally (lp/dispose-environment! env))))))

(deftest prose-and-empty-replies-consume-iterations-without-token-usage-test
  (doseq [reply [{:stop-reason :end :content "Premature done"} {:stop-reason :end}]]
    (let [env (environment)
          requests (atom 0)]

      (try (with-redefs [svar/ask-code! (fn [_ _]
                                          (when (> (swap! requests inc) 2)
                                            (throw (AssertionError. "Exceeded iteration budget")))
                                          reply)]
             (let [result (lp/run-turn! env "/goal --budget 2 Continue until verified" {})]
               (is (= :success (:status result)))
               (is (= 2 @requests))
               (is (= 2 (get (goals/check-goal env) "iterations_used")))
               (is (= 0 (get (goals/check-goal env) "tokens_used")))
               (is (= "budget_limited" (get (goals/check-goal env) "status")))))
           (finally (lp/dispose-environment! env))))))

(deftest repeated-empty-replies-never-leave-an-active-goal-test
  (let [limit @#'lp/CONSECUTIVE_EMPTY_REPLY_LIMIT]
    (doseq [budget [nil limit]]
      (let [env (environment)
            requests (atom 0)]

        (try
          (with-redefs [svar/ask-code! (fn [_ _]
                                         (when (> (swap! requests inc) limit)
                                           (throw (AssertionError.
                                                    "Repeated empty replies did not stop")))
                                         {:stop-reason :end})]
            (let [result (lp/run-turn!
                           env
                           (str "/goal " (when budget (str "--budget " budget " ")) "Verify work")
                           {})]
              (is (= limit @requests))
              (is (= limit (get (goals/check-goal env) "iterations_used")))
              (is (= (if budget "budget_limited" "paused") (get (goals/check-goal env) "status")))
              (is (= (if budget :success :error) (:status result)))))
          (finally (lp/dispose-environment! env)))))))

(deftest ordinary-turn-does-not-create-a-goal-test
  (let [env (environment)]
    (try (with-redefs [svar/ask-code! (fn [_ _]
                                        {:stop-reason :end :content "An ordinary answer."})]
           (is (= "An ordinary answer."
                  (get-in (lp/run-turn! env "Explain this" {}) [:answer :answer])))
           (is (nil? (goals/check-goal env))))
         (finally (lp/dispose-environment! env)))))

(deftest user-stop-pauses-active-goal-test
  (let [env (environment)]
    (try (with-redefs [svar/ask-code! (fn [& _]
                                        (throw (AssertionError. "Cancelled goal called provider")))]
           (let [result (lp/run-turn! env "/goal Stop this work" {:cancel-atom (atom true)})]
             (is (= :cancelled (:status result)))
             (is (= "paused" (get (goals/check-goal env) "status")))))
         (finally (lp/dispose-environment! env)))))
