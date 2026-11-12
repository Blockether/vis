(ns com.blockether.vis.internal.session.goals-boundary-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.goals :as goals]))

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
  (loop-env/create-environment
    (svar/make-router
      [{:id :fixture :api-key "test" :base-url "http://127.0.0.1:1/v1" :models [{:name "model"}]}])
    {:db :memory}))

(defdescribe
  slash-context-python-and-continuation-test
  (it
    "slash context python and continuation"
    (let [env
          (environment)

          requests
          (atom 0)

          snapshots
          (atom [])

          deletes
          (atom [])]

      (try
        (with-redefs [svar/ask-code!
                      (fn [_ opts]
                        (swap! deletes conj
                          (boolean (some #(str/includes? % "del session[\"goal_status\"]")
                                         (filter string? (tree-seq coll? seq (:messages opts))))))
                        (expect (str/includes? (str (:messages opts)) "update_goal"))
                        (expect (str/includes? (str (:messages opts)) "goal_status"))
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
                              "print(session['goal_status'])\nprint(update_goal('complete'))"}}]}

                          ;; The goal no longer directs work, so the model writes the final reply.
                          3
                          {:stop-reason :end
                           :content "All requested checks passed."
                           :api-usage {:input-tokens 7 :output-tokens 4}}

                          (throw (ex-info "Unexpected continuation" {}))))]
          (let [result (turn/run-turn! env "/goal Verify goal integration" {})]
            (expect (= "All requested checks passed." (get-in result [:answer :answer])))
            (expect (= 3 @requests))
            (expect (= "complete" (get (goals/check-goal env) "status")))
            ;; The final reply after the completion is not goal work.
            (expect (= 16 (get (goals/check-goal env) "tokens_used")))
            (expect (= 2 (get (goals/check-goal env) "iterations_used")))
            (expect (= "active" (get (first @snapshots) "session_goal_status")))
            ;; The model must see the status go away, not keep reading "active".
            (expect (= [false false true] @deletes))))
        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  iteration-budget-allows-last-tools-but-no-next-request-test
  (it "iteration budget allows last tools but no next request"
      ;; The shared slash boundary must enforce budgets after iOS substitutes a dash.
      (doseq [dash ["--" "—" "–"]]
        (let [env (environment)
              requests (atom 0)
              executions (atom [])
              execute @#'python-exec/execute-code]

          (try
            (with-redefs [python-exec/execute-code (fn [& args]
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

              (let [result (turn/run-turn! env (str "/goal Bounded task " dash "budget 1") {})]
                (expect (= :success (:status result)))
                (expect (= 1 @requests))
                (expect (= 2 (count @executions)))
                (expect (= 1 (get (goals/check-goal env) "iterations_used")))
                (expect (= "budget_limited" (get (goals/check-goal env) "status")))
                (expect (str/includes? (str result) "last iteration ran"))
                (expect (str/includes? (str result) "second tool in same iteration"))
                (expect (not (str/includes? (str result) "Goal token budget")))
                (expect (str/includes? (str (:answer result)) "iteration budget reached"))))
            (finally (loop-env/dispose-environment! env)))))))

(defdescribe
  terminal-goal-finishes-current-tools-without-next-request-test
  (it
    "terminal goal finishes current tools without next request"
    ;; #216: the current tools finish, then one final reply ends the turn. This holds with
    ;; unlimited, remaining or exhausted iteration budgets.
    (doseq [status
            ["complete" "blocked"]

            budget
            [nil 10 1]]

      (let [env
            (environment)

            requests
            (atom 0)

            final-request
            (atom nil)]

        (try
          (with-redefs [svar/ask-code!
                        (fn [_ opts]
                          (case (swap! requests inc)
                            1
                            {:stop-reason :tool-calls
                             :tool-calls [{:id "resolve"
                                           :name "python_execution"
                                           :input {:code (str "print(update_goal('" status "'))")}}
                                          {:id "finish-current-tools"
                                           :name "python_execution"
                                           :input {:code
                                                   "print('Current iteration tools finished.')"}}]}

                            2
                            (do (reset! final-request (filter
                                                        string?
                                                        (tree-seq coll? seq (:messages opts))))
                                {:stop-reason :end :content "Verified result or external blocker."})

                            (throw (AssertionError. "Unexpected request after the final reply"))))]
            (let [result (turn/run-turn!
                           env
                           (str "/goal " (when budget (str "--budget " budget " ")) "Resolve goal")
                           {})]
              (expect (= 2 @requests))
              ;; A normal final reply has no failure status.
              (expect (nil? (:status result)))
              (expect (= status (get (goals/check-goal env) "status")))
              (expect (= 1 (get (goals/check-goal env) "iterations_used")))
              (expect (= "Verified result or external blocker." (get-in result [:answer :answer])))
              (expect (str/includes? (str (:trace result)) "Current iteration tools finished."))
              ;; A complete goal leaves the view at once, so the final reply must not read it.
              (expect (= (= "complete" status)
                         (boolean (some #(str/includes? % "del session[\"goal_status\"]")
                                        @final-request)))
                      status)))
          (finally (loop-env/dispose-environment! env)))))))

(defdescribe
  prose-and-empty-replies-consume-iterations-without-token-usage-test
  (it "prose and empty replies consume iterations without token usage"
      (doseq [reply [{:stop-reason :end :content "Premature done"} {:stop-reason :end}]]
        (let [env (environment)
              requests (atom 0)]

          (try (with-redefs [svar/ask-code! (fn [_ _]
                                              (when (> (swap! requests inc) 2)
                                                (throw (AssertionError.
                                                         "Exceeded iteration budget")))
                                              reply)]
                 (let [result (turn/run-turn! env "/goal --budget 2 Continue until verified" {})]
                   (expect (= :success (:status result)))
                   (expect (= 2 @requests))
                   (expect (= 2 (get (goals/check-goal env) "iterations_used")))
                   (expect (= 0 (get (goals/check-goal env) "tokens_used")))
                   (expect (= "budget_limited" (get (goals/check-goal env) "status")))))
               (finally (loop-env/dispose-environment! env)))))))

(defdescribe
  repeated-empty-replies-never-leave-an-active-goal-test
  (it "repeated empty replies never leave an active goal"
      (let [limit @#'iteration/CONSECUTIVE_EMPTY_REPLY_LIMIT]
        (doseq [budget [nil limit]]
          (let [env (environment)
                requests (atom 0)]

            (try (with-redefs [svar/ask-code! (fn [_ _]
                                                (when (> (swap! requests inc) limit)
                                                  (throw (AssertionError.
                                                           "Repeated empty replies did not stop")))
                                                {:stop-reason :end})]
                   (let [result (turn/run-turn! env
                                                (str "/goal "
                                                     (when budget (str "--budget " budget " "))
                                                     "Verify work")
                                                {})]
                     (expect (= limit @requests))
                     (expect (= limit (get (goals/check-goal env) "iterations_used")))
                     (expect (= (if budget "budget_limited" "paused")
                                (get (goals/check-goal env) "status")))
                     (expect (= (if budget :success :error) (:status result)))))
                 (finally (loop-env/dispose-environment! env))))))))

(defdescribe
  ordinary-turn-does-not-create-a-goal-test
  (it "ordinary turn does not create a goal"
      (let [env (environment)]
        (try (with-redefs [svar/ask-code! (fn [_ _]
                                            {:stop-reason :end :content "An ordinary answer."})]
               (expect (= "An ordinary answer."
                          (get-in (turn/run-turn! env "Explain this" {}) [:answer :answer])))
               (expect (nil? (goals/check-goal env))))
             (finally (loop-env/dispose-environment! env))))))

(defdescribe
  finished-goal-leaves-later-turns-test
  (it "finished goal leaves later turns and keeps the system prefix"
      ;; User report: a completed goal still showed its state in later turns, so the model
      ;; treated a question as progress and kept working. A complete goal also takes its
      ;; rules out of the system message. A budget-limited goal keeps its status and rules,
      ;; so its cached prefix stays the same.
      (doseq [status ["complete" "budget_limited"]]
        (let [env (environment)
              requests (atom [])
              question (if (= "complete" status) 3 2)
              system-text #(pr-str (filterv (fn [m]
                                              (= "system" (:role m)))
                                     %))]

          (try
            (with-redefs [svar/ask-code!
                          (fn [_ opts]
                            (swap! requests conj (:messages opts))
                            (condp = (count @requests)
                              1 (if (= "complete" status)
                                  {:stop-reason :tool-calls
                                   :tool-calls [{:id "finish-goal"
                                                 :name "python_execution"
                                                 :input {:code "print(update_goal('complete'))"}}]}
                                  {:stop-reason :end :content "Progress so far."})
                              question {:stop-reason :end :content "Do you want a local fix only?"}
                              2 {:stop-reason :end :content "All checks passed."}
                              (throw (AssertionError. "A question must end the turn."))))]
              (turn/run-turn! env
                              (str "/goal "
                                   (when (= "budget_limited" status) "--budget 1 ")
                                   "Ship the finished objective")
                              {})
              (expect (= status (get (goals/check-goal env) "status")))
              (let [result (turn/run-turn! env "fix" {})
                    active-request (first @requests)
                    later-request (last @requests)
                    boundary (pr-str (last later-request))]

                (expect (= "Do you want a local fix only?" (get-in result [:answer :answer])))
                (expect (= question (count @requests)))
                (if (= "budget_limited" status)
                  (expect (= (system-text active-request) (system-text later-request)))
                  (expect (not (str/includes? (system-text later-request)
                                              "take the first rule that applies"))))
                ;; A complete goal leaves the session view; a budget-limited goal keeps its status.
                (if (= "budget_limited" status)
                  (expect (str/includes? boundary
                                         "session[\\\"goal_status\\\"] = \\\"budget_limited\\\""))
                  (expect (not (str/includes? boundary "goal_status"))))
                (expect (not (str/includes? boundary "goal_history")))
                (expect (= status (get (goals/check-goal env) "status")))))
            (finally (loop-env/dispose-environment! env)))))))

(defdescribe
  user-message-resumes-goal-before-model-request-test
  (it "user message resumes goal before model request"
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
                (goals/update-goal env "blocked"))
              (let [before (goals/check-goal env)]
                (with-redefs [svar/ask-code!
                              (fn [_ _]
                                (swap! snapshots conj
                                  [(get (ctx-loop/session-snapshot env) "session_goal_status")
                                   (goals/check-goal env)])
                                (case (swap! requests inc)
                                  1
                                  {:stop-reason :end :content "Premature answer"}

                                  2
                                  {:stop-reason :tool-calls
                                   :tool-calls [{:id "finish-resumed-goal"
                                                 :name "python_execution"
                                                 :input {:code "print(update_goal('complete'))"}}]}

                                  3
                                  {:stop-reason :end :content "All acceptance criteria verified."}

                                  (throw (AssertionError. "Unexpected goal continuation"))))]
                  (let [result (turn/run-turn! env "Use this new information and continue" {})
                        [model-status resumed] (first @snapshots)
                        preserved ["id" "objective" "iteration_budget" "iterations_used"]]

                    (expect (= "All acceptance criteria verified."
                               (get-in result [:answer :answer])))
                    (expect (= 3 @requests))
                    (expect (= "active" model-status))
                    (expect (= "active" (get resumed "status")))
                    (expect (= (inc (get before "version")) (get resumed "version")))
                    (expect (= (select-keys before preserved) (select-keys resumed preserved)))
                    (expect (= "complete" (get (goals/check-goal env) "status")))
                    (expect (= 3 (get (goals/check-goal env) "iterations_used")))))))
            (finally (loop-env/dispose-environment! env)))))))

(defdescribe
  non-user-and-cancelled-turns-do-not-resume-goal-test
  (it
    "non user and cancelled turns do not resume goal"
    (doseq [status
            ["paused" "blocked"]

            source
            [:council :cancelled :cancel-token]]

      (let [{:keys [db-info session-id] :as env}
            (environment)

            token
            (cancellation/cancellation-token)]

        (try (let [_ (goals/set-goal! db-info session-id "Keep the stopped goal" nil)]
               (if (= "paused" status)
                 (goals/control! db-info session-id :pause)
                 (goals/update-goal env "blocked"))
               (when (= :cancel-token source) (cancellation/cancel! token))
               (let [before (goals/check-goal env)]
                 (with-redefs [council/runtime (fn [_ sid]
                                                 (when (= :council source) {sid {:wake? true}}))
                               iteration/iteration-loop
                               (fn [turn-env _ _]
                                 (expect (= before (goals/check-goal turn-env)))
                                 {:status (if (= :council source) :success :cancelled)
                                  :answer {:answer "No goal work started."}
                                  :trace []
                                  :iteration-count 0
                                  :duration-ms 0})]

                   (turn/run-turn! env
                                   "Synthetic or cancelled input"
                                   (case source
                                     :council
                                     {}

                                     :cancelled
                                     {:cancel-atom (atom true)}

                                     :cancel-token
                                     {:cancel-token token}))
                   (expect (= before (goals/check-goal env))))))
             (finally (loop-env/dispose-environment! env)))))))

(defdescribe command-only-turns-do-not-resume-goal-test
             (it "command only turns do not resume goal"
                 (let [{:keys [db-info session-id] :as env} (environment)]
                   (try (let [_ (goals/set-goal! db-info session-id "Await user input" nil)
                              blocked (goals/update-goal env "blocked")]

                          (with-redefs [svar/ask-code! (fn [& _]
                                                         (throw
                                                           (AssertionError.
                                                             "Command-only turn called provider")))]
                            (doseq [request ["/goal" "/goal --unknown" "/goal --pause"]]
                              (turn/run-turn! env request {})
                              (expect (= blocked (goals/check-goal env))))
                            (turn/run-turn! env "/goal --cancel" {})
                            (expect (= "cancelled" (get (goals/check-goal env) "status")))))
                        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  user-stop-pauses-active-goal-test
  (it "user stop pauses active goal"
      (let [env (environment)]
        (try (with-redefs [svar/ask-code! (fn [& _]
                                            (throw (AssertionError.
                                                     "Cancelled goal called provider")))]
               (let [result (turn/run-turn! env "/goal Stop this work" {:cancel-atom (atom true)})]
                 (expect (= :cancelled (:status result)))
                 (expect (= "paused" (get (goals/check-goal env) "status")))))
             (finally (loop-env/dispose-environment! env))))))

(defdescribe ended-goal-adds-no-goal-rules-test
             (it "ended goal adds no goal rules to a later task"
                 ;; A later CLI or SDK task in the same session is an ordinary task.
                 (doseq [end ["complete" "cancelled" "cleared"]]
                   (let [{:keys [db-info session-id] :as env} (environment)
                         messages (atom nil)]

                     (try (goals/set-goal! db-info session-id "Finished objective" nil)
                          (case end
                            "complete"
                            (goals/update-goal env "complete")

                            "cancelled"
                            (goals/control! db-info session-id :cancel)

                            "cleared"
                            (goals/control! db-info session-id :clear))
                          (with-redefs [svar/ask-code! (fn [_ opts]
                                                         (reset! messages (str (:messages opts)))
                                                         {:stop-reason :end :content "Done."})]
                            (turn/run-turn! env "Ordinary task" {}))
                          (expect (not (str/includes? @messages "update_goal")) end)
                          (expect (not (str/includes? @messages "goal_status")) end)
                          (finally (loop-env/dispose-environment! env)))))))

(defdescribe active-goal-does-not-reject-progress-answer-test
             (it "active goal does not reject progress answer"
                 (let [{:keys [db-info session-id] :as env} (environment)]
                   (try (goals/set-goal! db-info session-id "Verify all remaining work" nil)
                        (expect (nil? (iteration/final-answer-gate-error
                                        env
                                        1
                                        []
                                        {:answer "Here is the progress so far."})))
                        (expect (= "active" (get (goals/check-goal env) "status")))
                        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  progress-answers-continue-without-validation-errors-or-finalization-test
  (it
    "progress answers continue without validation errors or finalization"
    (let [env
          (environment)

          progress-count
          (inc @#'iteration/CONSECUTIVE_EMPTY_REPLY_LIMIT)

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
        (with-redefs [ctx-loop/finalize-turn!
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
                          (cond (<= n progress-count) {:stop-reason :end
                                                       :content (str "Progress report " n ".")}
                                (= n (inc progress-count))
                                {:stop-reason :tool-calls
                                 :tool-calls [{:id "complete-goal"
                                               :name "python_execution"
                                               :input {:code "print(update_goal('complete'))"}}]}
                                (= n (+ 2 progress-count)) {:stop-reason :end
                                                            :content "All requested checks passed."}
                                :else (throw (AssertionError. "Unexpected goal continuation")))))]

          (let [result
                (turn/run-turn! env
                                "/goal Verify progress continuation"
                                {:hooks {:on-chunk #(swap! chunks conj %)}})

                progress
                (filterv :assistant-prose (:trace result))

                progress-chunks
                (filterv :assistant-prose @chunks)]

            (expect (= "All requested checks passed." (get-in result [:answer :answer])))
            (expect (= "complete" (get (goals/check-goal env) "status")))
            (expect (= (+ 2 progress-count) @requests))
            ;; The final reply after the completion is not goal work.
            (expect (= (dec @requests) (get (goals/check-goal env) "iterations_used")))
            (expect (= 1 @finalizations))
            (expect (every? #(and (zero? (:finalizations %)) (nil? (:answer %))) @request-states))
            (expect (every? #(= "active" (:goal-status %)) (take progress-count @request-states)))
            (expect (= (mapv #(str "Progress report " % ".") (range 1 (inc progress-count)))
                       (mapv :assistant-prose progress)))
            (expect (every? #(and (empty? (:blocks %)) (nil? (:answer %))) progress))
            (expect (= progress-count (count progress-chunks)))
            (expect (every? #(and (false? (:done? %)) (nil? (:final %))) progress-chunks))
            (doseq [n (range 1 (inc progress-count))]
              (expect (str/includes? (str (nth @messages n)) (str "Progress report " n ".")))
              (expect (str/includes? (str (nth @messages n)) "goal_continuation"))
              (expect (not (str/includes? (str (nth @messages n)) "Final answer rejected"))))))
        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  progress-answer-honors-last-iteration-budget-test
  (it "progress answer honors last iteration budget"
      (let [env
            (environment)

            requests
            (atom 0)]

        (try (with-redefs [svar/ask-code! (fn [_ _]
                                            (when (> (swap! requests inc) 1)
                                              (throw (AssertionError. "Exceeded progress budget")))
                                            {:stop-reason :end
                                             :content "Some work is verified; more remains."})]
               (let [result (turn/run-turn! env "/goal --budget 1 Verify remaining work" {})]
                 (expect (= :success (:status result)))
                 (expect (= 1 @requests))
                 (expect (= "budget_limited" (get (goals/check-goal env) "status")))
                 (expect (= "Some work is verified; more remains."
                            (:assistant-prose (first (:trace result)))))
                 (expect (empty? (:blocks (first (:trace result)))))
                 (expect (str/includes? (str (:answer result)) "iteration budget reached"))))
             (finally (loop-env/dispose-environment! env))))))

(defdescribe
  goal-objective-survives-fold-test
  (it "goal objective survives a fold of the turn that set it"
      ;; The model reads the objective to continue. A fold of the earlier turns must not remove it.
      (let [env
            (environment)

            requests
            (atom 0)

            messages
            (atom [])

            python
            (fn [id code]
              {:stop-reason :tool-calls
               :tool-calls [{:id id :name "python_execution" :input {:code code}}]})]

        (try (with-redefs [svar/ask-code!
                           (fn [_ opts]
                             (swap! messages conj (str (:messages opts)))
                             (case (swap! requests inc)
                               1
                               (python "block" "print(update_goal('blocked'))")

                               2
                               {:stop-reason :end :content "The access is missing."}

                               3
                               (python "fold"
                                       "print(fold_session('t1', 'Earlier work is settled'))")

                               4
                               (python "finish" "print(update_goal('complete'))")

                               5
                               {:stop-reason :end :content "The report is ready."}

                               (throw (AssertionError. "Unexpected request"))))]
               (turn/run-turn! env "/goal Ship the lighthouse report" {})
               (turn/run-turn! env "The access is ready now" {})
               (expect (= 5 @requests))
               (expect (= "complete" (get (goals/check-goal env) "status")))
               (expect (str/includes? (nth @messages 3) "Earlier work is settled"))
               (expect (str/includes? (nth @messages 3) "Ship the lighthouse report")))
             (finally (loop-env/dispose-environment! env))))))

(defdescribe
  goal-change-keeps-the-system-prefix-test
  (it "a new goal objective keeps the system prefix"
      ;; User report: the objective was in the system message, so a new /goal changed the prefix
      ;; and the provider wrote its cache of the whole conversation again.
      (let [env
            (environment)

            requests
            (atom [])

            system-text
            #(filterv (fn [m]
                        (= "system" (:role m)))
               %)]

        (try (with-redefs [svar/ask-code! (fn [_ opts]
                                            (swap! requests conj (:messages opts))
                                            {:stop-reason :end :content "Progress so far."})]
               (turn/run-turn! env "/goal --budget 1 Ship the lighthouse report" {})
               (turn/run-turn! env "/goal --budget 1 Ship the harbor report" {})
               (expect (= 2 (count @requests)))
               (expect (= "Ship the harbor report" (get (goals/check-goal env) "objective")))
               (expect (= (system-text (first @requests)) (system-text (last @requests))))
               (expect (str/includes? (pr-str (last (last @requests))) "Ship the harbor report")))
             (finally (loop-env/dispose-environment! env))))))
