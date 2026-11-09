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
