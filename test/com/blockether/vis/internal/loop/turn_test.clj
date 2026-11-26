(ns com.blockether.vis.internal.loop.turn-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

;; User report: a stored team card showed its leader with 0 s. The iteration loop returns no
;; `:duration-ms`, so every finished turn stored `duration_ms` 0.
(defdescribe
  turn-duration-test
  (it "stores the wall time of a finished turn"
      (let [router
            (svar/make-router [{:id :lmstudio
                                :api-key "test"
                                :base-url "http://127.0.0.1:1234/v1"
                                :models [{:name "model"}]}])

            environment
            (loop-env/create-environment router {:db :memory})

            db
            (:db-info environment)]

        (try (let [result
                   (with-redefs [iteration/iteration-loop
                                 (fn [& _]
                                   (Thread/sleep 40)
                                   {:status :success :answer "done" :iteration-count 1 :trace []})]
                     (turn/run-turn! environment "measure" {}))

                   stored
                   (some #(when (= (str (:id %)) (str (:session-turn-id result))) %)
                         (persistance/db-list-session-turns db (:session-id environment)))]

               (expect (= :success (:status result)))
               (expect (<= 40 (long (:duration-ms stored)) 10000) (pr-str stored)))
             (finally (loop-env/dispose-environment! environment))))))

(defdescribe
  council-turn-title-test
  (it "keeps an untitled session until a user sends a request"
      (let [environment (loop-env/create-environment ::router {:db :memory})
            db (:db-info environment)
            sid (:session-id environment)
            title #(:title (persistance/db-get-session db sid))
            starting-title (title)
            entry (-> (persistance/db-council-insert!
                        db {:author_sid "child" :activation_id "child-turn" :title "Review"
                            :content "Please review the changes." :created_at 1
                            :idempotency_key "review" :fingerprint "review"}
                        [] false)
                      :entry)]
        (try (with-redefs [iteration/iteration-loop
                           (fn [& _]
                             {:status :success :answer "done" :iteration-count 1 :trace []})]
               (expect (= :success
                          (:status (turn/run-turn! environment "Council notification #123"
                                                   {:request-kind :council
                                                    :council-entry-id (:entry_id entry)}))))
               (expect (= starting-title (title)))
               (turn/run-turn! environment "Review the changes" {})
               (expect (= "Review the changes" (title))))
             (finally (loop-env/dispose-environment! environment))))))

;; #456: an extension automation runs its callback as the turn. It must make no model
;; request: the iteration loop stays unused and the turn stores no tokens.
(defdescribe
  automation-turn-test
  (it
    "runs the callback instead of the model and stores its outcome"
    (let [environment
          (loop-env/create-environment ::router {:db :memory})

          payloads
          (atom [])

          model-calls
          (atom 0)]

      (swap! (:extensions environment) conj
        {:ext/name "iceberg"
         :ext/description "Lake upload."
         :ext/automations [{:key "sync"
                            :run (fn [_ payload]
                                   (swap! payloads conj payload)
                                   (get payload "ret"))}]})
      (try (with-redefs [iteration/iteration-loop (fn [& _]
                                                    (swap! model-calls inc)
                                                    (throw (ex-info "model" {})))]
             (let [run! (fn [ret]
                          (turn/turn! environment
                                      [(svar/user "Automation ran")]
                                      {:automation-run {:extension "iceberg"
                                                        :key "sync"
                                                        :timeout-ms 2000
                                                        :payload {"ret" ret}}}))
                   silent (run! nil)
                   answered (run! "Uploaded 3 sessions.")
                   failed (run!
                            {"marker" "slash_result" "status" "error" "title" "The lake is down."})
                   stored (last (sort-by :position
                                         (persistance/db-list-session-turns (:db-info environment)
                                                                            (:session-id
                                                                              environment))))]

               (expect (= 0 @model-calls))
               (expect (= 3 (count @payloads)))
               (expect (= [:success "[SILENT]"]
                          [(:status silent) (get-in silent [:answer 0 "markdown"])]))
               (expect (= "Uploaded 3 sessions." (get-in answered [:answer 0 "markdown"])))
               (expect (= {} (:tokens answered)))
               (expect (= :error (:status failed)))
               (expect (= :error (:status stored)))
               (expect (= "The lake is down." (get-in stored [:error "message"])))))
           (finally (loop-env/dispose-environment! environment)))))
  (it "fails the turn when the callback throws, ends late or is not declared"
      (let [environment (loop-env/create-environment ::router {:db :memory})]
        (swap! (:extensions environment) conj
          {:ext/name "iceberg"
           :ext/description "Lake upload."
           :ext/automations [{:key "boom"
                              :run (fn [_ _]
                                     (throw (ex-info "No token." {})))}
                             {:key "slow"
                              :run (fn [_ _]
                                     (Thread/sleep 5000))}]})
        (try (let [run! (fn [k]
                          (turn/run-turn!
                            environment
                            "Automation ran"
                            {:automation-run
                             {:extension "iceberg" :key k :timeout-ms 200 :payload {}}}))]
               (expect (= [:error "No token."] ((juxt :status :error) (run! "boom"))))
               (expect (re-find #"did not end" (:error (run! "slow"))))
               (expect (re-find #"does not declare" (:error (run! "gone")))))
             (finally (loop-env/dispose-environment! environment)))))
  ;; #490: a watchdog stop stored `automation_failed: java.lang.InterruptedException`.
  ;; The gateway gives the real failure through `:prepare-result`, before the turn is stored.
  (it "stores the result that the gateway prepares"
      (let [environment
            (loop-env/create-environment ::router {:db :memory})

            block
            {"type" "error" "code" "command_stalled" "message" "The command did not end."}]

        (swap! (:extensions environment) conj
          {:ext/name "iceberg"
           :ext/description "Lake upload."
           :ext/automations [{:key "sync"
                              :run (fn [_ _]
                                     "Done.")}]})
        (try (let [result
                   (turn/run-turn!
                     environment
                     "Automation ran"
                     {:hooks {:prepare-result #(assoc %
                                                 :status :error
                                                 :error block
                                                 :answer [block])}
                      :automation-run
                      {:extension "iceberg" :key "sync" :timeout-ms 2000 :payload {}}})

                   stored
                   (last (sort-by :position
                                  (persistance/db-list-session-turns (:db-info environment)
                                                                     (:session-id environment))))]

               (expect (= [:error "The command did not end."] ((juxt :status :error) result)))
               (expect (= [:error "command_stalled"]
                          [(:status stored) (get-in stored [:error "code"])])))
             (finally (loop-env/dispose-environment! environment)))))
  ;; #490: a cancel interrupted the wait of the turn, not the callback. The callback
  ;; continued with no owner, and the turn stored `java.lang.InterruptedException`
  ;; as the failure of the automation.
  (it
    "stops the callback and stores a cancelled turn when the turn is cancelled"
    (let [environment
          (loop-env/create-environment ::router {:db :memory})

          token
          (cancellation/cancellation-token)

          started
          (promise)

          stopped
          (promise)

          ended
          (promise)]

      (swap! (:extensions environment) conj
        {:ext/name "iceberg"
         :ext/description "Lake upload."
         :ext/automations
         [{:key "long"
           :run (fn [_ _]
                  (deliver started true)
                  (try (Thread/sleep 30000)
                       (catch InterruptedException e (deliver stopped true) (throw e))))}]})
      (try (let [worker (future (deliver ended
                                         (turn/run-turn! environment
                                                         "Automation ran"
                                                         {:cancel-token token
                                                          :automation-run {:extension "iceberg"
                                                                           :key "long"
                                                                           :timeout-ms 30000
                                                                           :payload {}}})))]
             (cancellation/on-cancel! token #(future-cancel worker))
             (expect (deref started 5000 false))
             (cancellation/cancel! token :client-cancel-turn)
             (let [result (deref ended 5000 nil)]
               (expect (deref stopped 2000 false))
               (expect (= [:cancelled :cancelled] ((juxt :status :prior-outcome) result)))
               (expect (not (re-find #"InterruptedException" (pr-str result))))))
           (finally (loop-env/dispose-environment! environment))))))

;; #514: a parked turn resumed under a later fold. The fold hid each new step, so the model
;; got the same request and ran the same step more than 1000 times.
(defdescribe
  repeated-request-test
  (it "stops the turn when its steps do not change the next request"
      (let [router
            (svar/make-router [{:id :lmstudio
                                :api-key "test"
                                :base-url "http://127.0.0.1:1234/v1"
                                :models [{:name "model"}]}])

            environment
            (loop-env/create-environment router {:db :memory})

            requests
            (atom 0)]

        (try (let [result
                   (with-redefs [transcript/apply-summaries (fn [_ _] [])
                                 svar/ask-code! (fn [_ _]
                                                  (when (> (swap! requests inc) 10)
                                                    (throw (AssertionError. "The turn loops")))
                                                  {:stop-reason :tool-calls
                                                   :tool-calls [{:id "step"
                                                                 :name "py3"
                                                                 :input {:code "print('step')"}}]})]
                     (turn/run-turn! environment "do the work" {}))]

               (expect (= 3 @requests))
               (expect (= :error (:status result)))
               (expect (str/includes? (str (:answer result)) "same request")))
             (finally (loop-env/dispose-environment! environment))))))
