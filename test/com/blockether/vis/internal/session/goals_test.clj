(ns com.blockether.vis.internal.session.goals-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- environment [] {:db-info (h/store) :session-id (h/store-session! (h/store) {:channel :api})})

(defn- rejected? [f] (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(defdescribe
  explicit-goals-test
  (it "explicit goals"
      (let [{:keys [db-info session-id] :as env} (environment)]
        (expect (nil? (goals/check-goal env)))
        (expect (nil? (goals/continuation-prompt env)))
        (let [goal (goals/set-goal! db-info session-id "  Verify the SDK  " 100)]
          (expect (document/valid-json? "gateway" "session_goal" goal))
          (expect (= 100 (get goal "iteration_budget")))
          (expect (= 0 (get goal "iterations_used")))
          (expect (not (contains? goal "token_budget")))
          (expect (= goal (goals/check-goal env)))
          (expect (= "Verify the SDK" (get goal "objective")))
          (expect (= "active" (get goal "status")))
          (expect (string? (goals/continuation-prompt env)))
          (expect (false? (persistence/db-compare-session-goal! db-info session-id 0 goal)))
          (let [done
                (goals/update-goal env (get goal "id") 1 "complete" "SDK boundary tests pass.")]
            (expect (= "complete" (get done "status")))
            (expect (nil? (goals/continuation-prompt env)))
            (expect (= 2 (get done "revision")))
            (expect (= 2 (get done "version"))))))))

(defdescribe goal-blocker-audit-policy-test
             (it "goal blocker audit policy"
                 (let [{:keys [db-info session-id] :as env} (environment)]
                   (goals/set-goal! db-info session-id "Verify the full objective" nil)
                   (doseq [prompt [goals/prompt (goals/continuation-prompt env)]]
                     (doseq [instruction
                             ["3 consecutive goal continuations" "verified live operation"
                              "no progress" "user input, resume, or new progress"
                              "attempted alternatives" "not an independent engine check"]]
                       (expect (str/includes? prompt instruction))))
                   (expect (str/includes? (goals/continuation-prompt env)
                                          "Your reply was delivered as progress"))
                   (goals/control! db-info session-id :pause)
                   (expect (nil? (goals/continuation-prompt env))))))

(defdescribe
  user-controls-and-stale-work-test
  (it "user controls and stale work"
      (let [{:keys [db-info session-id] :as env}
            (environment)

            first-goal
            (goals/set-goal! db-info session-id "First" nil)]

        (expect (= "paused" (get (goals/control! db-info session-id :pause) "status")))
        (expect (rejected?
                  #(goals/update-goal env (get first-goal "id") 1 "complete" "Stale evidence")))
        (expect (= "active" (get (goals/control! db-info session-id :resume) "status")))
        (goals/account! env first-goal {:input-tokens 10 :output-tokens 5})
        (expect (= 0 (get (goals/check-goal env) "tokens_used")))
        (let [replacement (goals/set-goal! db-info session-id "Second" nil)]
          (expect (not= (get first-goal "id") (get replacement "id")))
          (expect (= 4 (get replacement "revision")))
          (expect (rejected?
                    #(goals/update-goal env (get first-goal "id") 1 "blocked" "Old blocker")))
          (expect (= "cancelled" (get (goals/control! db-info session-id :cancel) "status")))
          (expect (rejected? #(goals/control! db-info session-id :resume)))))))

(defdescribe
  user-turn-resume-keeps-nonresumable-goals-unchanged-test
  (it
    "user turn resume keeps nonresumable goals unchanged"
    (expect (nil? (goals/resume-for-user-turn! (environment))))
    (doseq [status [:active :complete :cancelled :budget-limited :paused-exhausted
                    :blocked-exhausted]]
      (let [{:keys [db-info session-id] :as env} (environment)
            goal (goals/set-goal! db-info session-id "Preserve the goal and its budget" 1)]

        (when (contains? #{:budget-limited :paused-exhausted :blocked-exhausted} status)
          (goals/account! env goal nil))
        (case status
          :active
          nil

          :complete
          (goals/update-goal env (get goal "id") (get goal "version") "complete" "Verified.")

          :cancelled
          (goals/control! db-info session-id :cancel)

          :budget-limited
          (goals/request-halt-result env goal)

          :paused-exhausted
          (goals/control! db-info session-id :pause)

          :blocked-exhausted
          (goals/update-goal env (get goal "id") (get goal "version") "blocked" "Need user input."))
        (let [before (goals/check-goal env)]
          (expect (= before (goals/resume-for-user-turn! env)))
          (expect (= before (goals/check-goal env))))))))

(defdescribe user-turn-resume-preserves-concurrent-user-controls-test
             (it "user turn resume preserves concurrent user controls"
                 (doseq [action [:cancel :replace :pause]]
                   (let [{:keys [db-info session-id] :as env} (environment)
                         compare-goal! persistence/db-compare-session-goal!
                         intervened? (atom false)
                         concurrent (atom nil)]

                     (goals/set-goal! db-info session-id "Original goal" nil)
                     (goals/control! db-info session-id :pause)
                     (with-redefs [persistence/db-compare-session-goal!
                                   (fn [& args]
                                     (when (compare-and-set! intervened? false true)
                                       (reset! concurrent
                                         (case action
                                           :cancel
                                           (goals/control! db-info session-id :cancel)

                                           :replace
                                           (goals/set-goal! db-info session-id "New goal" nil)

                                           :pause
                                           (do (goals/control! db-info session-id :resume)
                                               (goals/control! db-info session-id :pause)))))
                                     (apply compare-goal! args))]
                       (let [result (goals/resume-for-user-turn! env)]
                         (expect @intervened?)
                         (expect (= @concurrent result))
                         (expect (= @concurrent (goals/check-goal env)))))))))

(defdescribe
  iteration-budget-test
  (it "iteration budget"
      (let [{:keys [db-info session-id] :as env}
            (environment)

            goal
            (goals/set-goal! db-info session-id "Bounded work" 1)]

        (goals/account! env goal {:input-tokens 10 :output-tokens 7 :cache-read-tokens 8})
        (expect (= "active" (get (goals/check-goal env) "status")))
        (expect (nil? (goals/halt-result env goal)))
        (expect (some? (goals/request-halt-result env goal)))
        (let [limited (goals/check-goal env)]
          (expect (= 1 (get limited "iterations_used")))
          (expect (= 17 (get limited "tokens_used")))
          (expect (= (- (get limited "updated_at") (get goal "updated_at"))
                     (get limited "time_used_ms")))
          (expect (= "budget_limited" (get limited "status")))
          (expect (nil? (goals/continuation-prompt env)))
          (expect (rejected? #(goals/control! db-info session-id :resume)))
          (expect (rejected? #(goals/update-goal env (get goal "id") 1 "complete" "Too late")))
          (expect (= 1 (get limited "version")))))))

(defdescribe iteration-usage-survives-resume-and-reset-is-explicit-test
             (it "iteration usage survives resume and reset is explicit"
                 (let [{:keys [db-info session-id] :as env}
                       (environment)

                       goal
                       (goals/set-goal! db-info session-id "Bounded" 3)]

                   (goals/account! env goal nil)
                   (goals/control! db-info session-id :pause)
                   (let [resumed (goals/control! db-info session-id :resume)]
                     (expect (= 1 (get resumed "iterations_used")))
                     (expect (= 3 (get resumed "iteration_budget")))
                     (goals/account! env resumed nil)
                     (expect (= 2 (get (goals/check-goal env) "iterations_used")))
                     (expect (nil? (goals/request-halt-result env resumed)))
                     (goals/account! env resumed nil)
                     (expect (some? (goals/request-halt-result env resumed))))
                   (let [replacement (goals/set-goal! db-info session-id "New scope" nil)]
                     (expect (= 0 (get replacement "iterations_used")))
                     (expect (nil? (get replacement "iteration_budget")))
                     (expect (not (document/valid-json? "gateway"
                                                        "session_goal"
                                                        (assoc replacement
                                                          "token_budget" 100))))))))

(defdescribe
  slash-parsing-test
  (it "slash parsing"
      (let [{:keys [db-info session-id]}
            (environment)

            invoke
            #(goals/slash! {:db-info db-info :session/id session-id :command/raw %})]

        (expect (= :ok
                   (:slash/status (invoke "/goal --budget 40 -- Keep \"quotes\"
and newlines"))))
        (expect (= "Keep \"quotes\"
and newlines"
                   (get (goals/check-goal db-info session-id) "objective")))
        (expect (= 40 (get (goals/check-goal db-info session-id) "iteration_budget")))
        (expect (false? (get-in (invoke "/goal --pause") [:slash/data :goal-run?])))
        (expect (true? (get-in (invoke "/goal --resume") [:slash/data :goal-run?])))
        (doseq [raw ["/goal" "/goal --budget 0 work" "/goal --budget 999999999999999999999 work"
                     "/goal --budget -1 work" "/goal --unknown"]]
          (expect (= :error (:slash/status (invoke raw))) raw)))))

(defdescribe trailing-budget-parsing-test
             (it "trailing budget parsing"
                 (let [{:keys [db-info session-id] :as env}
                       (environment)

                       invoke
                       #(goals/slash! {:db-info db-info :session/id session-id :command/raw %})]

                   ;; A trailing budget must not silently become an unlimited objective.
                   (doseq [[raw objective budget]
                           [["/goal Verify the change --budget 100" "Verify the change" 100]
                            ["/goal Keep \"quotes\"
and newlines    --budget        40"
                             "Keep \"quotes\"
and newlines" 40]
                            ["/goal -- Keep --budget 100" "Keep --budget 100" nil]
                            ["/goal --budget 3 -- Keep --budget 100" "Keep --budget 100" 3]
                            ["/goal Explain \"--budget 100\"" "Explain \"--budget 100\"" nil]]]
                     (expect (= :ok (:slash/status (invoke raw))) raw)
                     (expect (= objective (get (goals/check-goal env) "objective")) raw)
                     (expect (= budget (get (goals/check-goal env) "iteration_budget")) raw))
                   (doseq [raw ["/goal Work --budget" "/goal Work --budget 0"
                                "/goal Work --budget -1" "/goal Work --budget many"
                                "/goal Work --budget 1.5" "/goal Work --budget 9007199254740992"
                                "/goal Work --budget 999999999999999999999"
                                "/goal --budget 2 Work --budget 3"]]
                     (let [before (goals/check-goal env)]
                       (expect (= :error (:slash/status (invoke raw))) raw)
                       (expect (= before (goals/check-goal env)) raw))))))

;; Regression: iOS smart punctuation turns the two hyphens in goal flags into a dash.
(defdescribe smart-punctuation-flags-test
             (it "smart punctuation flags"
                 (let [{:keys [db-info session-id] :as env}
                       (environment)

                       invoke
                       #(goals/slash! {:db-info db-info :session/id session-id :command/raw %})

                       objective
                       "Keep — prose, – ranges, \"quotes\"
and --code intact"]

                   (doseq [dash
                           ["--" "—" "–"]

                           raw
                           [(str "/goal " dash "budget 3 " objective)
                            (str "/goal " objective " " dash "budget 3")]]

                     (expect (= :ok (:slash/status (invoke raw))) raw)
                     (let [goal (goals/check-goal env)]
                       (expect (= objective (get goal "objective")) raw)
                       (expect (= 3 (get goal "iteration_budget")) raw)
                       (doseq [[flag status run?] [["pause" "paused" false] ["resume" "active" true]
                                                   ["cancel" "cancelled" false]]]
                         (let [result (invoke (str "/goal " dash flag))
                               updated (goals/check-goal env)]

                           (expect (= :ok (:slash/status result)))
                           (expect (= run? (get-in result [:slash/data :goal-run?])))
                           (expect (= status (get updated "status")))
                           (expect (= (get goal "id") (get updated "id")))
                           (expect (= objective (get updated "objective")))
                           (expect (= 3 (get updated "iteration_budget"))))))))))

(defdescribe
  smart-punctuation-keeps-objective-literal-test
  (it
    "smart punctuation keeps objective literal"
    (let [{:keys [db-info session-id] :as env}
          (environment)

          invoke
          #(goals/slash! {:db-info db-info :session/id session-id :command/raw %})]

      (doseq [dash
              ["—" "–"]

              [args objective budget]
              [[(str "-- Keep " dash "budget 100") (str "Keep " dash "budget 100") nil]
               [(str dash "budget 3 -- " dash "pause") (str dash "pause") 3]
               [(str "Explain \"" dash "budget 100\"") (str "Explain \"" dash "budget 100\"") nil]
               [(str "Keep word" dash "budget 100") (str "Keep word" dash "budget 100") nil]
               [(str "Keep " dash "pause in prose") (str "Keep " dash "pause in prose") nil]
               [(str dash " Leading punctuation") (str dash " Leading punctuation") nil]]]

        (let [raw (str "/goal " args)]
          (expect (= :ok (:slash/status (invoke raw))) raw)
          (expect (= objective (get (goals/check-goal env) "objective")) raw)
          (expect (= budget (get (goals/check-goal env) "iteration_budget")) raw)))
      (doseq [dash
              ["—" "–"]

              args
              [(str dash "budget") (str dash "budget 3") (str "Work " dash "budget")
               (str "Work " dash "budget 0") (str dash "budget -1 Work")
               (str "Work " dash "budget many") (str "Work " dash "budget 1.5")
               (str "Work " dash "budget 9007199254740992") (str "--budget 2 Work " dash "budget 3")
               (str dash "budget 2 Work --budget 3")]]

        (let [before
              (goals/check-goal env)

              raw
              (str "/goal " args)]

          (expect (= :error (:slash/status (invoke raw))) raw)
          (expect (= before (goals/check-goal env)) raw))))))

(defdescribe
  goal-time-measures-active-wall-clock-test
  (it
    "goal time measures active wall clock"
    (let [{:keys [db-info session-id] :as env}
          (environment)

          now
          (atom 1000)]

      (with-redefs [util/now-ms (fn ^long []
                                  (long @now))]
        (let [goal (goals/set-goal! db-info session-id "Measure the whole goal" nil)]
          (reset! now 6000)
          (goals/account! env goal nil)
          (expect (= 5000 (get (goals/check-goal env) "time_used_ms")))
          ;; Time in tools between model responses belongs to the goal as well.
          (reset! now 9000)
          (expect (= 8000 (get (goals/control! db-info session-id :pause) "time_used_ms")))
          (reset! now 90000)
          (let [resumed (goals/control! db-info session-id :resume)]
            (expect (= 8000 (get resumed "time_used_ms")))
            (reset! now 95000)
            (expect (= 13000
                       (get (goals/update-goal env
                                               (get resumed "id")
                                               (get resumed "version")
                                               "blocked"
                                               "Awaiting input.")
                            "time_used_ms")))))
        (reset! now 100000)
        (let [resumed (goals/control! db-info session-id :resume)]
          (reset! now 105000)
          (let [done (goals/update-goal env
                                        (get resumed "id")
                                        (get resumed "version")
                                        "complete"
                                        "All checks passed.")]
            (expect (= 18000 (get done "time_used_ms")))
            (reset! now 200000)
            (goals/account! env done nil)
            (expect (= 18000 (get (goals/check-goal env) "time_used_ms")))
            (expect (= 18000 (get (goals/control! db-info session-id :cancel) "time_used_ms")))))
        (expect (= 0 (get (goals/set-goal! db-info session-id "New goal" nil) "time_used_ms")))))))

(defdescribe goal-time-stops-at-budget-cancellation-and-failure-test
             (it "goal time stops at budget cancellation and failure"
                 (doseq [stop [:budget :cancel :error]]
                   (let [{:keys [db-info session-id] :as env} (environment)
                         now (atom 1000)]

                     (with-redefs [util/now-ms (fn ^long []
                                                 (long @now))]
                       (let [goal (goals/set-goal! db-info session-id "Stop the clock" 1)]
                         (reset! now 6000)
                         (goals/account! env goal nil)
                         (reset! now 9000)
                         (case stop
                           :budget
                           (goals/request-halt-result env goal)

                           :cancel
                           (goals/control! db-info session-id :cancel)

                           :error
                           (goals/finish-turn! env goal :error))
                         (let [stopped (goals/check-goal env)]
                           (expect (= 8000 (get stopped "time_used_ms")))
                           (reset! now 200000)
                           (goals/request-halt-result env goal)
                           (expect (= stopped (goals/check-goal env))))))))))

(defdescribe model-cannot-create-or-expand-test
             (it "model cannot create or expand"
                 (let [{:keys [db-info session-id] :as env} (environment)]
                   (expect (rejected? #(goals/update-goal env "missing" 1 "complete" "No goal")))
                   (let [goal (goals/set-goal! db-info session-id "Original scope" nil)]
                     (doseq [status ["active" "paused" "cancelled"]]
                       (expect (rejected? #(goals/update-goal env (get goal "id") 1 status "No"))))
                     (expect (rejected? #(goals/update-goal env (get goal "id") 1 "complete" "  ")))
                     (expect (= goal (goals/check-goal env)))))))

(defdescribe mutation-events-test
             (it "mutation events"
                 (let [{:keys [db-info session-id] :as env}
                       (environment)

                       seen
                       (atom [])

                       listener
                       (goals/add-listener! #(swap! seen conj [%1 %2]))]

                   (try (let [goal (goals/set-goal! db-info session-id "Notify" nil)]
                          (goals/check-goal env)
                          (expect (= [[session-id goal]] @seen))
                          (goals/control! db-info session-id :cancel)
                          (expect (= [1 2] (mapv #(get (second %) "revision") @seen))))
                        (finally (goals/remove-listener! listener))))))

(defdescribe stopped-turn-cannot-pause-a-replacement-test
             (it "stopped turn cannot pause a replacement"
                 (let [{:keys [db-info session-id] :as env}
                       (environment)

                       first-goal
                       (goals/set-goal! db-info session-id "First" nil)]

                   (goals/finish-turn! env first-goal :cancelled)
                   (expect (= "paused" (get (goals/check-goal env) "status")))
                   (let [replacement (goals/set-goal! db-info session-id "Replacement" nil)]
                     (goals/finish-turn! env first-goal :error)
                     (expect (= replacement (goals/check-goal env)))
                     (expect (= :cancelled (:status (goals/halt-result env first-goal))))))))
