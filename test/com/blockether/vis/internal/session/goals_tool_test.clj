(ns com.blockether.vis.internal.session.goals-tool-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- environment [] {:db-info (h/store) :session-id (h/store-session! (h/store) {:channel :api})})

(defn- bindings
  [env]
  {'update_goal
   (fn [& args]
     (extension/invoke-symbol-wrapper foundation/vis-extension (first goals/symbols) args env))})

;; Regression #200: checking persisted state alone missed a failure after the write.
(defdescribe update-goal-envelope-test
             (it "update goal envelope"
                 (doseq [status ["complete" "blocked"]]
                   (let [{:keys [db-info session-id] :as env} (environment)
                         _ (goals/set-goal! db-info session-id "Verify the goal result" nil)
                         entry (first goals/symbols)
                         result ((:ext.symbol/fn entry) env status)
                         after (goals/check-goal env)]

                     (expect (= 'update_goal (:ext.symbol/symbol entry)))
                     (expect (false? (get-in entry [:ext.symbol/activity :show-start])))
                     (expect (some #{entry}
                                   (get-in foundation/vis-extension
                                           [:ext/engine :ext.engine/symbols])))
                     (expect (= "(status)" (extension/symbol-signature entry)))
                     (expect (extension/tool-result? result))
                     (expect (true? (:success? result)))
                     (expect (= {"goal_status" status} (:result result)))
                     (expect (= status (get after "status")))
                     (expect (not (contains? after "reason")))
                     (expect (= 2 (get after "version")))
                     (expect (= 2 (get after "revision")))))))

(defdescribe
  update-goal-python-call-shapes-test
  (it "update goal python call shapes"
      ;; #200 also rejected keyword calls advertised by the signature.
      (doseq [worker? [false true]]
        (let [{:keys [db-info session-id] :as env} (environment)]
          (tpc/with-own
            [ctx (bindings env) nil {:worker? worker?}]
            (doseq [status ["complete" "blocked"]
                    call [(pr-str status) (str "status=" (pr-str status))]]

              (let [before (goals/set-goal! db-info session-id "Verify Python goal calls" nil)
                    result (ep/run-python-block ctx
                                                (str "print(json.dumps(update_goal(" call ")))")
                                                "t1/i1")
                    after (goals/check-goal env)
                    label (str "worker=" worker? ", call=" call)]

                (expect (nil? (:error result)) (str label "\n" (:error result)))
                (when (nil? (:error result))
                  (expect (= {"op" "update_goal" "goal_status" status}
                             (json/read-json (:stdout result) :key-fn identity))
                          label))
                (expect (= status (get after "status")) label)
                (expect (= 2 (get after "version")) label)
                (expect (= (inc (get before "revision")) (get after "revision")) label))))))))

(defdescribe
  update-goal-python-rejections-do-not-write-test
  (it "update goal python rejections do not write"
      (doseq [worker? [false true]]
        (let [{:keys [db-info session-id] :as env} (environment)
              before (goals/set-goal! db-info session-id "Preserve the active goal" nil)]

          (tpc/with-own
            [ctx (bindings env) nil {:worker? worker?}]
            ;; The old call form with an id, a version and a reason is refused too.
            (doseq [args ["" "'complete', 'extra'" "'complete', status='blocked'"
                          "status='complete', extra=True" "'active'" "'paused'" "'cancelled'"
                          "goal_id='g', version=1, status='complete', reason='Checked'"]]
              (let [result (ep/run-python-block ctx (str "update_goal(" args ")") "t1/i1")]
                (expect (some? (:error result)) (str "worker=" worker? ", args=" args))
                (expect (= before (goals/check-goal env)) (str "worker=" worker? ", args=" args))))
            (let [result (ep/run-python-block ctx "update_goal('blocked')" "t1/i2")
                  after (goals/check-goal env)
                  retry-result (ep/run-python-block ctx "update_goal('blocked')" "t1/i3")]

              (expect (nil? (:error result)) (str (:error result)))
              (expect (some? (:error retry-result)))
              (expect (= after (goals/check-goal env)))
              (expect (= 2 (get after "version")))))))))

(defdescribe
  update-goal-activity-outcome-test
  (it "update goal activity outcome"
      (doseq [status ["complete" "blocked" nil]]
        (let [{:keys [db-info session-id] :as env} (environment)
              _ (when status (goals/set-goal! db-info session-id "Verify goal Activity" nil))
              events (atom [])
              result (try (binding [extension/*tool-event-sink* #(swap! events conj %)]
                            ((get (bindings env) 'update_goal) (or status "blocked")))
                          (catch clojure.lang.ExceptionInfo e e))
              [start terminal] @events]

          (expect (= [:start :terminal] (mapv :phase @events)))
          (expect (false? (:show-start start)))
          (expect (= [:mutation :mutation] (mapv :classification @events)))
          (expect (= :update_goal (:operation terminal)))
          (if status
            (do (expect (true? (:succeeded terminal)))
                (expect (= {"op" "update_goal" "goal_status" status} result))
                (expect (= "Updated goal" (get-in terminal [:presentation "headline"])))
                (expect (= (str/capitalize status) (get-in terminal [:presentation "summary"]))))
            (do (expect (true? (:failed terminal)))
                (expect (str/includes? (ex-message result) "No goal is active"))
                (expect (nil? (goals/check-goal env)))))))))
