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
(defdescribe
  update-goal-envelope-test
  (it "update goal envelope"
      (doseq [status ["complete" "blocked"]]
        (let [{:keys [db-info session-id] :as env} (environment)
              before (goals/set-goal! db-info session-id "Verify the goal result" nil)
              entry (first goals/symbols)
              result ((:ext.symbol/fn entry)
                       env
                       (get before "id")
                       (get before "version")
                       status
                       "  Verified evidence or concrete blocker.  ")
              after (goals/check-goal env)]

          (expect (= 'update_goal (:ext.symbol/symbol entry)))
          (expect (false? (get-in entry [:ext.symbol/activity :show-start])))
          (expect (some #{entry}
                        (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols])))
          (expect (= "(goal_id, version, status, reason)" (extension/symbol-signature entry)))
          (expect (every? :required? (:ext.symbol/params entry)))
          (expect (str/includes? (extension/symbol-doc-text entry) "mixed calls are supported"))
          (expect (extension/tool-result? result))
          (expect (true? (:success? result)))
          (expect (= after (:result result)))
          (expect (= status (get after "status")))
          (expect (= "Verified evidence or concrete blocker." (get after "reason")))
          (expect (= 2 (get after "version")))
          (expect (= 2 (get after "revision")))))))

(defdescribe
  update-goal-python-call-shapes-test
  (it
    "update goal python call shapes"
    ;; #200 also rejected mixed positional/keyword calls advertised by the signature.
    (doseq [worker? [false true]]
      (let [{:keys [db-info session-id] :as env} (environment)]
        (tpc/with-own
          [ctx (bindings env) nil {:worker? worker?}]
          (doseq [status ["complete" "blocked"]
                  positional-count (range 5)]

            (let [before (goals/set-goal! db-info session-id "Verify Python goal calls" nil)
                  parameters [["goal_id" "g['id']"] ["version" "g['version']"]
                              ["status" (pr-str status)] ["reason" "'  Checked evidence.  '"]]
                  args (concat (map second (take positional-count parameters))
                               (map (fn [[k v]]
                                      (str k "=" v))
                                    (reverse (drop positional-count parameters))))
                  _ (ep/bind-ctx! ctx {"goal" before})
                  result (ep/run-python-block ctx
                                              (str "g = session['goal']\n"
                                                   "r = update_goal(" (str/join ", " args)
                                                   ")\n" "print(json.dumps({k: r[k] for k in g}))")
                                              "t1/i1")
                  after (goals/check-goal env)]

              (expect (nil? (:error result))
                      (str "worker=" worker?
                           ", status=" status
                           ", positional=" positional-count
                           "\n" (:error result)))
              (when (nil? (:error result))
                (expect
                  (= after (json/read-json (:stdout result) :key-fn identity))
                  (str "worker=" worker? ", status=" status ", positional=" positional-count)))
              (expect (= status (get after "status"))
                      (str "worker=" worker? ", status=" status ", positional=" positional-count))
              (expect (= "Checked evidence." (get after "reason"))
                      (str "worker=" worker? ", status=" status ", positional=" positional-count))
              (expect (= 2 (get after "version"))
                      (str "worker=" worker? ", status=" status ", positional=" positional-count))
              (expect
                (= (inc (get before "revision")) (get after "revision"))
                (str "worker=" worker? ", status=" status ", positional=" positional-count)))))))))

(defdescribe
  update-goal-python-rejections-do-not-write-test
  (it
    "update goal python rejections do not write"
    (doseq [worker? [false true]]
      (let [{:keys [db-info session-id] :as env} (environment)
            before (goals/set-goal! db-info session-id "Preserve the active goal" nil)]

        (tpc/with-own
          [ctx (bindings env) nil {:worker? worker?}]
          (ep/bind-ctx! ctx {"goal" before})
          (doseq [args ["" "g['id'], g['version'], 'complete', 'Checked', 'extra'"
                        "g['id'], g['version'], 'complete', status='blocked', reason='Duplicate'"
                        "g['id'], g['version'], 'complete'"
                        "g['id'], g['version'], status='complete', reason='Checked', extra=True"
                        "g['id'], g['version'], 'complete', 'Checked', goal_id=g['id']"
                        "goal_id=g['id'], status='complete', reason='Checked'"
                        "g['id'], g['version'], 'active', 'Not permitted'"
                        "g['id'], g['version'], 'complete', '  '"
                        "'wrong-id', g['version'], 'blocked', 'Stale identity'"
                        "g['id'], g['version'] + 1, 'blocked', 'Stale version'"]]
            (let [result (ep/run-python-block ctx
                                              (str "g = session['goal']\nupdate_goal(" args ")")
                                              "t1/i1")]
              (expect (some? (:error result)) (str "worker=" worker? ", args=" args))
              (expect (= before (goals/check-goal env)) (str "worker=" worker? ", args=" args))))
          (let [result (ep/run-python-block
                         ctx
                         "update_goal(g['id'], g['version'], 'blocked', 'Needs external input')"
                         "t1/i2")
                after (goals/check-goal env)
                retry-result (ep/run-python-block
                               ctx
                               "update_goal(g['id'], g['version'], 'blocked', 'Retry')"
                               "t1/i3")]

            (expect (nil? (:error result)) (str (:error result)))
            (expect (some? (:error retry-result)))
            (expect (= after (goals/check-goal env)))
            (expect (= 2 (get after "version")))))))))

(defdescribe update-goal-activity-outcome-test
             (it "update goal activity outcome"
                 (doseq [status ["complete" "blocked" nil]]
                   (let [{:keys [db-info session-id] :as env} (environment)
                         before (when status
                                  (goals/set-goal! db-info session-id "Verify goal Activity" nil))
                         events (atom [])
                         result (try (binding [extension/*tool-event-sink* #(swap! events conj %)]
                                       ((get (bindings env) 'update_goal)
                                         (get before "id" "missing")
                                         1
                                         (or status "blocked")
                                         "Verified evidence"))
                                     (catch clojure.lang.ExceptionInfo e e))
                         [start terminal] @events]

                     (expect (= [:start :terminal] (mapv :phase @events)))
                     (expect (false? (:show-start start)))
                     (expect (= [:mutation :mutation] (mapv :classification @events)))
                     (expect (= :update_goal (:operation terminal)))
                     (if status
                       (do (expect (true? (:succeeded terminal)))
                           (expect (= status (get result "status")))
                           (expect (= "Updated goal" (get-in terminal [:presentation "headline"])))
                           (expect (= (str (str/capitalize status) " · Verify goal Activity")
                                      (get-in terminal [:presentation "summary"]))))
                       (do (expect (true? (:failed terminal)))
                           (expect (str/includes? (ex-message result) "stopped or changed"))
                           (expect (nil? (goals/check-goal env)))))))))
