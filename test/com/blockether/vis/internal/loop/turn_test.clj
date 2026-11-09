(ns com.blockether.vis.internal.loop.turn-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistance]
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
             (finally (loop-env/dispose-environment! environment))))))
