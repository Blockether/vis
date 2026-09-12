(ns com.blockether.vis.internal.session.goals-boundary-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [lazytest.core :refer [around-each set-ns-context!]]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
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

(deftest user-message-resumes-goal-before-model-request-test
  ;; A follow-up must resume the existing goal, not silently become a one-reply turn.
  (doseq [status ["paused" "blocked"]]
    (let [{:keys [db-info session-id] :as env} (environment)
          requests (atom 0)
          snapshots (atom [])]

      (try
        (let [goal (goals/set-goal! db-info session-id "Verify all acceptance criteria" 10)]
          (goals/account! env goal {:input-tokens 10 :output-tokens 5})
          (if (= "paused" status)
            (goals/control! db-info session-id :pause)
            (goals/update-goal env
                               (get goal "id")
                               (get goal "version")
                               "blocked"
                               "Need user input."))
          (let [before (goals/check-goal env)]
            (with-redefs
              [svar/ask-code!
               (fn [_ _]
                 (swap! snapshots conj (get (ctx-loop/session-snapshot env) "session_goal"))
                 (case (swap! requests inc)
                   1
                   {:stop-reason :end :content "Premature answer"}

                   2
                   {:stop-reason :tool-calls
                    :tool-calls
                    [{:id "finish-resumed-goal"
                      :name "python_execution"
                      :input
                      {:code
                       "g = session['goal']\nprint(update_goal(g['id'], g['version'], 'complete', 'All acceptance criteria verified.'))"}}]}

                   3
                   {:stop-reason :end :content "Verified and complete."}

                   (throw (AssertionError. "Unexpected goal continuation"))))]
              (let [result (lp/run-turn! env "Use this new information and continue" {})
                    resumed (first @snapshots)
                    preserved ["id" "objective" "iteration_budget" "iterations_used" "tokens_used"
                               "time_used_ms" "created_at"]]

                (is (= "Verified and complete." (get-in result [:answer :answer])))
                (is (= 3 @requests))
                (is (= "active" (get resumed "status")))
                (is (nil? (get resumed "reason")))
                (is (= (inc (get before "version")) (get resumed "version")))
                (is (= (inc (get before "revision")) (get resumed "revision")))
                (is (= (select-keys before preserved) (select-keys resumed preserved)))
                (is (= "complete" (get (goals/check-goal env) "status")))
                (is (= 4 (get (goals/check-goal env) "iterations_used")))))))
        (finally (lp/dispose-environment! env))))))

(deftest non-user-and-cancelled-turns-do-not-resume-goal-test
  (doseq [status
          ["paused" "blocked"]

          source
          [:council :cancelled :cancel-token]]

    (let [{:keys [db-info session-id] :as env}
          (environment)

          token
          (cancellation/cancellation-token)]

      (try (let [goal (goals/set-goal! db-info session-id "Keep the stopped goal" nil)]
             (if (= "paused" status)
               (goals/control! db-info session-id :pause)
               (goals/update-goal env
                                  (get goal "id")
                                  (get goal "version")
                                  "blocked"
                                  "Need user input."))
             (when (= :cancel-token source) (cancellation/cancel! token))
             (let [before (goals/check-goal env)]
               (with-redefs [council/runtime (fn [_ sid]
                                               (when (= :council source) {sid {:wake? true}}))
                             lp/iteration-loop (fn [turn-env _ _]
                                                 (is (= before (goals/check-goal turn-env)))
                                                 {:status
                                                  (if (= :council source) :success :cancelled)
                                                  :answer {:answer "No goal work started."}
                                                  :trace []
                                                  :iteration-count 0
                                                  :duration-ms 0})]

                 (lp/run-turn! env
                               "Synthetic or cancelled input"
                               (case source
                                 :council
                                 {}

                                 :cancelled
                                 {:cancel-atom (atom true)}

                                 :cancel-token
                                 {:cancel-token token}))
                 (is (= before (goals/check-goal env))))))
           (finally (lp/dispose-environment! env))))))

(deftest command-only-turns-do-not-resume-goal-test
  (let [{:keys [db-info session-id] :as env} (environment)]
    (try (let [goal (goals/set-goal! db-info session-id "Await user input" nil)
               blocked (goals/update-goal env
                                          (get goal "id")
                                          (get goal "version")
                                          "blocked"
                                          "Need user input.")]

           (with-redefs [svar/ask-code! (fn [& _]
                                          (throw (AssertionError.
                                                   "Command-only turn called provider")))]
             (doseq [request ["/goal" "/goal --unknown" "/goal --pause"]]
               (lp/run-turn! env request {})
               (is (= blocked (goals/check-goal env))))
             (lp/run-turn! env "/goal --cancel" {})
             (is (= "cancelled" (get (goals/check-goal env) "status")))))
         (finally (lp/dispose-environment! env)))))

(deftest user-stop-pauses-active-goal-test
  (let [env (environment)]
    (try (with-redefs [svar/ask-code! (fn [& _]
                                        (throw (AssertionError. "Cancelled goal called provider")))]
           (let [result (lp/run-turn! env "/goal Stop this work" {:cancel-atom (atom true)})]
             (is (= :cancelled (:status result)))
             (is (= "paused" (get (goals/check-goal env) "status")))))
         (finally (lp/dispose-environment! env)))))

(deftest active-goal-does-not-reject-progress-answer-test
  (let [{:keys [db-info session-id] :as env} (environment)]
    (try (goals/set-goal! db-info session-id "Verify all remaining work" nil)
         (is (nil? (lp/final-answer-gate-error env 1 [] {:answer "Here is the progress so far."})))
         (is (= "active" (get (goals/check-goal env) "status")))
         (finally (lp/dispose-environment! env)))))

(deftest progress-answers-continue-without-validation-errors-or-finalization-test
  (let [env
        (environment)

        progress-count
        (inc @#'lp/CONSECUTIVE_EMPTY_REPLY_LIMIT)

        requests
        (atom 0)

        messages
        (atom [])

        request-states
        (atom [])

        chunks
        (atom [])

        finalizations
        (atom 0)

        finalize
        ctx-loop/finalize-turn!]

    (try
      (with-redefs
        [ctx-loop/finalize-turn!
         (fn [& args]
           (swap! finalizations inc)
           (apply finalize args))

         svar/ask-code!
         (fn [_ opts]
           (swap! request-states conj
             {:finalizations @finalizations
              :answer (:answer (ctx-loop/read-turn-state env))
              :goal-status (get (goals/check-goal env) "status")})
           (swap! messages conj (:messages opts))
           (let [n (swap! requests inc)]
             (cond
               (<= n progress-count) {:stop-reason :end :content (str "Progress report " n ".")}
               (= n (inc progress-count))
               {:stop-reason :tool-calls
                :tool-calls
                [{:id "complete-goal"
                  :name "python_execution"
                  :input
                  {:code
                   "g = session['goal']\nprint(update_goal(g['id'], g['version'], 'complete', 'All requested checks passed.'))"}}]}
               (= n (+ progress-count 2)) {:stop-reason :end :content "Verified and complete."}
               :else (throw (AssertionError. "Unexpected goal continuation")))))]

        (let [result
              (lp/run-turn! env
                            "/goal Verify progress continuation"
                            {:hooks {:on-chunk #(swap! chunks conj %)}})

              progress
              (filterv :assistant-prose (:trace result))

              progress-chunks
              (filterv :assistant-prose @chunks)]

          (is (= "Verified and complete." (get-in result [:answer :answer])))
          (is (= "complete" (get (goals/check-goal env) "status")))
          (is (= (+ progress-count 2) @requests))
          (is (= @requests (get (goals/check-goal env) "iterations_used")))
          (is (= 1 @finalizations))
          (is (every? #(and (zero? (:finalizations %)) (nil? (:answer %))) @request-states))
          (is (every? #(= "active" (:goal-status %)) (take progress-count @request-states)))
          (is (= (mapv #(str "Progress report " % ".") (range 1 (inc progress-count)))
                 (mapv :assistant-prose progress)))
          (is (every? #(and (empty? (:blocks %)) (nil? (:answer %))) progress))
          (is (= progress-count (count progress-chunks)))
          (is (every? #(and (false? (:done? %)) (nil? (:final %))) progress-chunks))
          (doseq [n (range 1 (inc progress-count))]
            (is (str/includes? (str (nth @messages n)) (str "Progress report " n ".")))
            (is (str/includes? (str (nth @messages n)) "goal_continuation"))
            (is (not (str/includes? (str (nth @messages n)) "Final answer rejected"))))))
      (finally (lp/dispose-environment! env)))))

(deftest progress-answer-honors-last-iteration-budget-test
  (let [env
        (environment)

        requests
        (atom 0)]

    (try (with-redefs [svar/ask-code! (fn [_ _]
                                        (when (> (swap! requests inc) 1)
                                          (throw (AssertionError. "Exceeded progress budget")))
                                        {:stop-reason :end
                                         :content "Some work is verified; more remains."})]
           (let [result (lp/run-turn! env "/goal --budget 1 Verify remaining work" {})]
             (is (= :success (:status result)))
             (is (= 1 @requests))
             (is (= "budget_limited" (get (goals/check-goal env) "status")))
             (is (= "Some work is verified; more remains."
                    (:assistant-prose (first (:trace result)))))
             (is (empty? (:blocks (first (:trace result)))))
             (is (str/includes? (str (:answer result)) "iteration budget reached"))))
         (finally (lp/dispose-environment! env)))))
