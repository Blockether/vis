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
         (fn [_ _]
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
          (is (= 16 (get (goals/check-goal env) "tokens_used")))
          (is (str/includes? (str @snapshots) "Verify goal integration"))))
      (finally (lp/dispose-environment! env)))))

(deftest budget-stops-tools-and-provider-continuation-test
  (let [env
        (environment)

        requests
        (atom 0)]

    (try (with-redefs [lp/execute-code
                       (fn [& _]
                         (throw (AssertionError. "Budget allowed tool work")))

                       svar/ask-code!
                       (fn [_ _]
                         (swap! requests inc)
                         {:stop-reason :tool-calls
                          :api-usage {:input-tokens 10 :output-tokens 5}
                          :tool-calls [{:id "no-work"
                                        :name "python_execution"
                                        :input {:code "print('must-not-execute')"}}]})]

           (let [result (lp/run-turn! env "/goal --budget 1 Bounded task" {})]
             (is (= :success (:status result)))
             (is (= 1 @requests))
             (is (= "budget_limited" (get (goals/check-goal env) "status")))
             (is (str/includes? (str (:answer result)) "budget reached"))))
         (finally (lp/dispose-environment! env)))))

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
